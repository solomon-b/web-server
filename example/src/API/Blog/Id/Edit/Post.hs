module API.Blog.Id.Edit.Post where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Errors (NotFound (..), Unauthorized (..), throwErr)
import Control.Applicative ((<|>))
import Control.Monad (unless, void)
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (FromJSON, ToJSON)
import Data.Base64.Types qualified as Base64
import Data.Bool (bool)
import Data.ByteString.Base64.URL qualified as Base64.Url
import Data.ByteString.Char8 qualified as Char8
import Data.Has (Has)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..), RecordInstance (..), display)
import Data.Text.Encoding qualified as TE
import Data.Time (getCurrentTime)
import Deriving.Aeson qualified as Deriving
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Queries.UserWithMetadata (FullUser (..))
import Effects.Database.Tables.BlogPosts (Domain (dAuthorId))
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.Images qualified as Images
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import GHC.Generics (Generic)
import Hasql.Interpolate (OneRow (..))
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import OrphanInstances.MultipartData (Extension (..), lookupFilePathMaybe, readInputMaybe)
import OrphanInstances.OneRow ()
import OrphanInstances.Servant ()
import Servant ((:>))
import Servant qualified
import Servant.Multipart (FromMultipart, MultipartForm, Tmp)
import Servant.Multipart qualified as Multipart
import System.Directory qualified as Dir
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> "blog"
    :> Servant.Capture "id" BlogPosts.Id
    :> "edit"
    :> MultipartForm Tmp EditPost
    :> Servant.PostAccepted '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] Servant.NoContent)

--------------------------------------------------------------------------------

-- | DTO type for editing a blog post.
data EditPost = EditPost
  { title :: BlogPosts.Subject,
    content :: BlogPosts.Body,
    published :: Bool,
    heroImagePath :: Maybe (Extension, FilePath)
  }
  deriving stock (Generic)
  deriving (Display) via (RecordInstance EditPost)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.CamelToSnake]] EditPost

instance FromMultipart Tmp EditPost where
  fromMultipart :: Multipart.MultipartData Tmp -> Either String EditPost
  fromMultipart form = do
    title <- BlogPosts.Subject <$> Multipart.lookupInput "title" form
    content <- BlogPosts.Body <$> Multipart.lookupInput "content" form
    published <- fromMaybe False <$> readInputMaybe @Bool "published" form
    let heroImagePath = lookupFilePathMaybe "heroImagePath" form

    pure EditPost {..}

--------------------------------------------------------------------------------

handler ::
  ( MonadReader env m,
    Has OTEL.Tracer env,
    Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  Auth.Authz ->
  BlogPosts.Id ->
  EditPost ->
  m
    ( Servant.Headers
        '[ Servant.Header "HX-Redirect" Text
         ]
        Servant.NoContent
    )
handler (Auth.Authz FullUser {fuId = userId, fuIsAdmin} _) bid req@EditPost {..} = do
  Observability.handlerSpan "POST /blog/new" req display $ do
    BlogPosts.Domain {dAuthorId, dHeroImage} <- maybe (throwErr NotFound) (pure . BlogPosts.toDomain) =<< execQuerySpanThrow (BlogPosts.getBlogPost bid)
    unless (fuIsAdmin || userId == dAuthorId) $ throwErr Unauthorized
    let oldHeroImageId = fmap Images.dId dHeroImage
    heroImageId <- traverse (insertImage userId) heroImagePath
    publishedAt <- bool (pure Nothing) (Just <$> liftIO getCurrentTime) published
    void $ execQuerySpanThrow $ BlogPosts.updateBlogPost $ BlogPosts.Model bid dAuthorId title content publishedAt (heroImageId <|> oldHeroImageId)
    pure $ Servant.addHeader ("/blog/" <> display bid) Servant.NoContent

insertImage ::
  ( MonadIO m,
    Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    MonadReader env m,
    Has OTEL.Tracer env,
    MonadUnliftIO m
  ) =>
  User.Id ->
  (Extension, FilePath) ->
  m Images.Id
insertImage uid fp = do
  path' <- copyFile fp
  OneRow imageId <- execQuerySpanThrow $ Images.insertImage $ Images.ModelInsert uid "Hero Image" path'
  pure imageId

copyFile :: (MonadIO m) => (Extension, FilePath) -> m Text
copyFile (Extension ext, path) = liftIO $ do
  let name = encodeFilename path
      destination = "/static/images/" <> name <> Text.unpack ext
  Dir.copyFile path $ "./server" <> destination
  pure $ Text.pack destination

encodeFilename :: FilePath -> FilePath
encodeFilename = Char8.unpack . Base64.extractBase64 . Base64.Url.encodeBase64' . SHA256.hash . TE.encodeUtf8 . Text.pack
