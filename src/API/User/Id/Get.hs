module API.User.Id.Get where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadThrow)
import Domain.Types.User (User)
import Effects.Database.Class (MonadDB)
import Effects.Database.Queries.User (selectUser)
import Effects.Database.Tables.User qualified as User
import Errors (Forbidden (..), throwErr)
import Log qualified
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

type Route = "user" :> Servant.Capture "id" User.Id :> Servant.Get '[Servant.JSON] User

--------------------------------------------------------------------------------

handler ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m
  ) =>
  User.Id ->
  m User
handler uid =
  selectUser uid >>= \case
    Nothing -> throwErr Forbidden
    Just user -> pure user
