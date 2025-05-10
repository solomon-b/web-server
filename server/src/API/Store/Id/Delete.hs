module API.Store.Id.Delete where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Errors (Unauthorized (..), throwErr)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text.Display (display)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.Products qualified as Products
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import Log qualified
import OpenTelemetry.Trace qualified as Trace
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> "blog"
    :> Servant.Capture "id" Products.Id
    :> Servant.Delete '[Servant.JSON] ()

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
  Products.Id ->
  m ()
handler (Auth.Authz User.Domain {dIsAdmin} _) pid =
  Observability.handlerSpan "GET /blog/:id" pid display $ do
    unless dIsAdmin (throwErr Unauthorized)
    execQuerySpanThrow $ Products.delete pid
