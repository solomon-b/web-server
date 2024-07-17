module API.User.Delete where

--------------------------------------------------------------------------------

import Auth qualified
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text.Display (display)
import Domain.Types.User (User (..))
import Effects.Database.Class (MonadDB)
import Effects.Database.Queries.User (deleteUser)
import Effects.Database.Tables.User qualified as User
import Errors (throw401')
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
    MonadDB m,
    MonadUnliftIO m
  ) =>
  Auth.Authz ->
  User.Id ->
  m ()
handler (Auth.Authz User {userId, userIsAdmin} _) uid =
  Tracing.handlerSpan "/user/:id/delete" () display $ do
    unless (userId == uid || userIsAdmin) throw401'
    deleteUser uid
