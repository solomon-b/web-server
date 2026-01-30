module API.User.Logout.Post
  ( Route,
    handler,
  )
where

import App.Auth qualified as Auth
import App.Errors (InternalServerError (..), throwErr)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Data.Text qualified as Text
import Effects.Database.Tables.ServerSessions qualified as Session
import Hasql.Pool qualified as HSQL
import Log qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> "user"
    :> "logout"
    :> Servant.Post
         '[HTML]
         ( Servant.Headers
             '[Servant.Header "Location" Text]
             Servant.NoContent
         )

--------------------------------------------------------------------------------

handler ::
  ( Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool env,
    MonadThrow m,
    MonadIO m
  ) =>
  Auth.Authz ->
  m
    ( Servant.Headers
        '[Servant.Header "Location" Text]
        Servant.NoContent
    )
handler Auth.Authz {authzSession} = do
  Auth.expireServerSession (Session.mSessionId authzSession) >>= \case
    Left err -> do
      throwErr $ InternalServerError $ Text.pack $ show err
    Right _ ->
      pure $ Servant.addHeader "/" Servant.NoContent
