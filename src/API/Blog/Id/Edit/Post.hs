module API.Blog.Id.Edit.Post where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Errors (Unauthorized (..), throwErr, NotFound (..))
import Control.Monad (unless, void)
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Has (Has)
import Data.Text (Text)
import Data.Text.Display (Display (..), RecordInstance (..), display)
import Deriving.Aeson qualified as Deriving
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import GHC.Generics (Generic)
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import OrphanInstances.OneRow ()
import OrphanInstances.Servant ()
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Web.FormUrlEncoded qualified as FormUrlEncoded
import Effects.Database.Tables.BlogPosts (Domain(dAuthorId))

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> "blog"
    :> Servant.Capture "id" BlogPosts.Id
    :> "edit"
    :> Servant.ReqBody '[Servant.FormUrlEncoded] EditPost
    :> Servant.PostAccepted '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] Servant.NoContent)

--------------------------------------------------------------------------------

data EditPost = EditPost
  { title :: Text,
    content :: Text,
    published :: Bool,
    heroImagePath :: Maybe Text
  }
  deriving stock (Generic)
  deriving (Display) via (RecordInstance EditPost)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.CamelToSnake]] EditPost

instance FormUrlEncoded.FromForm EditPost where
  fromForm f =
    EditPost
      <$> FormUrlEncoded.parseUnique "title" f
      <*> FormUrlEncoded.parseUnique "content" f
      <*> FormUrlEncoded.parseUnique "published" f
      <*> FormUrlEncoded.parseUnique "heroImagePath" f

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
handler (Auth.Authz User.Domain {..} _) bid req@EditPost {..} = do
  Observability.handlerSpan "POST /blog/new" req display $ do
    BlogPosts.Domain {dAuthorId} <- maybe (throwErr NotFound) (pure . BlogPosts.toDomain) =<< execQuerySpanThrow (BlogPosts.getBlogPost bid)
    unless (dIsAdmin || dId == dAuthorId) $ throwErr Unauthorized
    void $ execQuerySpanThrow $ BlogPosts.updateBlogPost $ BlogPosts.ModelUpdate bid title content published heroImagePath
    pure $ Servant.addHeader ("/blog/" <> display bid) Servant.NoContent
