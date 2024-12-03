module API.Blog.New.Post where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Errors (Unauthorized (..), throwErr)
import Control.Monad (unless)
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

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> "blog"
    :> "new"
    :> Servant.ReqBody '[Servant.FormUrlEncoded] CreatePost
    :> Servant.PostAccepted '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] Servant.NoContent)

--------------------------------------------------------------------------------

data CreatePost = CreatePost
  { title :: Text,
    content :: Text
  }
  deriving stock (Generic)
  deriving (Display) via (RecordInstance CreatePost)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.CamelToSnake]] CreatePost

instance FormUrlEncoded.FromForm CreatePost where
  fromForm f =
    CreatePost
      <$> FormUrlEncoded.parseUnique "title" f
      <*> FormUrlEncoded.parseUnique "content" f

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
handler (Auth.Authz User.Domain {..} _) req@CreatePost {..} = do
  Observability.handlerSpan "POST /blog/new" req display $ do
    unless dIsAdmin $ throwErr Unauthorized
    bid <- execQuerySpanThrow $ BlogPosts.insertBlogPost $ BlogPosts.ModelInsert dId title content False Nothing
    pure $ Servant.addHeader ("/blog/" <> display bid) Servant.NoContent
