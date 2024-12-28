module API.Blog.New.Get where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Errors (Unauthorized (..), throwErr)
import Component.Forms.BlogPost (template)
import Component.Frame (loadFrameWithNav)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Data.Text.Display (display)
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML, RawHtml (..), renderLucid)

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> Servant.Header "HX-Request" Bool
    :> "blog"
    :> "new"
    :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "Vary" Text] RawHtml)

--------------------------------------------------------------------------------

handler ::
  ( Applicative m,
    Has Trace.Tracer env,
    MonadCatch m,
    Log.MonadLog m,
    MonadUnliftIO m,
    MonadReader env m
  ) =>
  Auth.Authz ->
  Maybe Bool ->
  m (Servant.Headers '[Servant.Header "Vary" Text] RawHtml)
handler (Auth.Authz user@User.Domain {..} _) hxTrigger =
  Observability.handlerSpan "GET /post/new" () (display . Servant.getResponse) $ do
    unless dIsAdmin $ throwErr Unauthorized

    let formFragment = template Nothing Nothing Nothing False Nothing
    fullPage <- loadFrameWithNav (Auth.IsLoggedIn user) "blog-tab" formFragment

    case hxTrigger of
      Just True ->
        pure $ Servant.addHeader "HX-Request" $ RawHtml $ Lucid.renderBS formFragment
      _ -> do
        let html = renderLucid fullPage
        pure $ Servant.addHeader "HX-Request" html
