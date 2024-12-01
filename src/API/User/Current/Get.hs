module API.User.Current.Get where

--------------------------------------------------------------------------------

import App.Auth (Authz (..))
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text.Display (display)
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

type Route = Servant.AuthProtect "cookie-auth" :> "user" :> "current" :> Servant.Get '[Servant.JSON] User.Domain

--------------------------------------------------------------------------------

handler ::
  ( Log.MonadLog m,
    MonadThrow m,
    MonadReader env m,
    Has OTEL.Tracer env,
    MonadCatch m,
    MonadUnliftIO m
  ) =>
  Authz ->
  m User.Domain
handler (Authz user _) =
  Observability.handlerSpan "GET /user/current" () display $ pure user
