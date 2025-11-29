module API.User.Id.Get where

--------------------------------------------------------------------------------

import App.Errors (Forbidden (..), throwErr)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text.Display (display)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute qualified as Execute
import Effects.Database.Queries.UserWithMetadata (FullUser)
import Effects.Database.Queries.UserWithMetadata qualified as UserQuery
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import Log qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

type Route = "user" :> Servant.Capture "id" User.Id :> Servant.Get '[Servant.JSON] FullUser

--------------------------------------------------------------------------------

handler ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    MonadReader env m,
    Has Tracer env,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  User.Id ->
  m FullUser
handler uid =
  Observability.handlerSpan "GET /user/:id" uid display $ do
    Execute.execQuerySpan (UserQuery.getFullUser uid) >>= \case
      Right (Just user) -> pure user
      _ -> throwErr Forbidden
