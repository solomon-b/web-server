module API.Blog.Delete where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Errors (NotFound (..), Unauthorized (..), throwErr)
import Control.Monad (forM_, unless)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Data.Text.Display (display)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import Log qualified
import OpenTelemetry.Trace qualified as Trace
import Servant ((:>))
import Servant qualified
import Web.FormUrlEncoded (FromForm (..))
import Web.FormUrlEncoded qualified as FormUrlEncoded

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> "blog"
    :> "delete"
    :> Servant.ReqBody '[Servant.FormUrlEncoded] Deletions
    :> Servant.Post '[Servant.JSON] (Servant.Headers '[Servant.Header "HX-Redirect" Text] ())

--------------------------------------------------------------------------------

handler ::
  forall m env.
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    MonadReader env m,
    Has Trace.Tracer env,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  Auth.Authz ->
  Deletions ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] ())
handler (Auth.Authz User.Domain {dIsAdmin} _) (Deletions pids) =
  Observability.handlerSpan "GET /blog/:id" pids (display . Servant.getResponse) $ do
    unless (dIsAdmin) (throwErr Unauthorized)

    forM_ pids $ execQuerySpanThrow . BlogPosts.deleteBlogPost

    pure $ Servant.addHeader "/admin/blog" ()

newtype Deletions = Deletions {getdeletions :: [BlogPosts.Id]}

instance FromForm Deletions where
  fromForm f =
    Deletions <$> FormUrlEncoded.parseAll "ids" f
