module API.User.Current where

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
import Tracing (handlerSpan)

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
  handlerSpan "/user/current" () display $ pure user
