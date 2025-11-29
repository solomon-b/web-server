module API.Blog.New.Post where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (blogNewGetLink)
import App.Auth qualified as Auth
import App.Errors (Unauthorized (..), throwErr)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (FromJSON, ToJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Base64.Types qualified as Base64
import Data.ByteString.Base64.URL qualified as Base64.Url
import Data.ByteString.Char8 qualified as Char8
import Data.Has (Has)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..), RecordInstance (..), display)
import Data.Text.Encoding qualified as TE
import Data.Validation (Validation (..))
import Deriving.Aeson qualified as Deriving
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Queries.UserWithMetadata (FullUser (..))
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
import Web.HttpApiData qualified as Http

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> "blog"
    :> "new"
    :> MultipartForm Tmp CreatePost
    :> Servant.PostAccepted '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] Servant.NoContent)

--------------------------------------------------------------------------------

data CreatePost = CreatePost
  { title :: BlogPosts.Subject,
    content :: BlogPosts.Body,
    published :: Bool,
    heroImagePath :: Maybe (Extension, FilePath)
  }
  deriving stock (Generic)
  deriving (Display) via (RecordInstance CreatePost)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.CamelToSnake]] CreatePost

instance FromMultipart Tmp CreatePost where
  fromMultipart form = do
    title <- BlogPosts.Subject <$> Multipart.lookupInput "title" form
    content <- BlogPosts.Body <$> Multipart.lookupInput "content" form
    published <- fromMaybe False <$> readInputMaybe @Bool "published" form
    let heroImagePath = lookupFilePathMaybe "heroImagePath" form

    pure CreatePost {..}

data ValidationError = MissingSubject | MissingBody

instance Display ValidationError where
  displayBuilder = \case
    MissingSubject -> "Subject is missing."
    MissingBody -> "Body is missing."

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
  CreatePost ->
  m
    ( Servant.Headers
        '[ Servant.Header "HX-Redirect" Text
         ]
        Servant.NoContent
    )
handler (Auth.Authz FullUser {fuId = userId, fuIsAdmin} _) req@CreatePost {..} = do
  Observability.handlerSpan "POST /blog/new" req display $ do
    unless fuIsAdmin $ throwErr Unauthorized
    case validateRequest req of
      Failure err -> do
        Log.logInfo "POST /blog/new Request validation failure" (Aeson.object ["request" .= req, "validationErrors" .= display err])
        pure $ Servant.addHeader ("/" <> Http.toUrlPiece blogNewGetLink) Servant.NoContent
      Success () -> do
        heroImageId <- traverse (insertImage userId) heroImagePath
        bid <- execQuerySpanThrow $ BlogPosts.insertBlogPost $ BlogPosts.Insert userId title content published heroImageId
        pure $ Servant.addHeader ("/blog/" <> display bid) Servant.NoContent

--------------------------------------------------------------------------------

validateTitle :: BlogPosts.Subject -> Validation [ValidationError] ()
validateTitle BlogPosts.Subject {..} =
  if Text.null getSubject then Failure [MissingSubject] else Success ()

validateBody :: BlogPosts.Body -> Validation [ValidationError] ()
validateBody BlogPosts.Body {..} =
  if Text.null getBody then Failure [MissingBody] else Success ()

validateRequest :: CreatePost -> Validation [ValidationError] ()
validateRequest CreatePost {..} =
  validateTitle title *> validateBody content

--------------------------------------------------------------------------------

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
