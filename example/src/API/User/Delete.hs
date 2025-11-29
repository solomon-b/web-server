module API.User.Delete where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Errors (Unauthorized (..), throwErr)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text.Display (display)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Queries.UserWithMetadata (FullUser (..))
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

type Route = Servant.AuthProtect "cookie-auth" :> "user" :> Servant.Capture "id" User.Id :> "delete" :> Servant.Delete '[Servant.JSON] ()

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
handler (Auth.Authz FullUser {fuId, fuIsAdmin} _) uid =
  Observability.handlerSpan "DELETE /user/:id/delete" uid display $ do
    unless (fuId == uid || fuIsAdmin) (throwErr Unauthorized)
    execQuerySpanThrow $ User.deleteUser uid
