module API.User.Delete where

--------------------------------------------------------------------------------

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
import Servant.Auth.Server qualified as SAS
import Tracing (handlerSpan)

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
  SAS.AuthResult User ->
  User.Id ->
  m ()
handler (SAS.Authenticated User {userId, userIsAdmin}) uid =
  handlerSpan "/user/:id/delete" () display $ do
    unless (userId == uid || userIsAdmin) throw401'
    deleteUser uid
handler _ _ = throw401'
