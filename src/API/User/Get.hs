module API.User.Get where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadThrow)
import Domain.Types.User (User)
import Effects.Database.Class (MonadDB)
import Effects.Database.Queries.User (selectUsers)
import Log qualified
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

type Route = "user" :> Servant.Get '[Servant.JSON] [User]

--------------------------------------------------------------------------------

handler ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m
  ) =>
  m [User]
handler = selectUsers
