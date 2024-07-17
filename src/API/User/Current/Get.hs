module API.User.Current.Get where

--------------------------------------------------------------------------------

import Auth (Authz (..))
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text.Display (display)
import Domain.Types.User (User)
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import Tracing qualified

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
  m User
handler (Authz user _) =
  Tracing.handlerSpan "/user/current" () display $ pure user
