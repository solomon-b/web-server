module API.Store.New.Get where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Errors (Unauthorized (..), throwErr)
import Component.Forms.Product (template)
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
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> Servant.Header "HX-Request" Bool
    :> "store"
    :> "new"
    :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "Vary" Text] (Lucid.Html ()))

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
  m (Servant.Headers '[Servant.Header "Vary" Text] (Lucid.Html ()))
handler (Auth.Authz user@User.Domain {..} _) hxTrigger =
  Observability.handlerSpan "GET /store/new" () (display . Servant.getResponse) $ do
    unless dIsAdmin $ throwErr Unauthorized

    let formFragment = template Nothing Nothing Nothing Nothing Nothing Nothing Nothing False
    fullPage <- loadFrameWithNav (Auth.IsLoggedIn user) "store-tab" formFragment

    case hxTrigger of
      Just True ->
        pure $ Servant.addHeader "HX-Request" formFragment
      _ ->
        pure $ Servant.addHeader "HX-Request" fullPage
