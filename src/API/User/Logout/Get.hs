module API.User.Logout.Get where

--------------------------------------------------------------------------------

import Auth qualified
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Effects.Database.Class (MonadDB)
import Effects.Database.Tables.ServerSessions qualified as Session
import Effects.Observability qualified as Observability
import Errors (InternalServerError (..), throwErr)
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import Servant ((:>))
import Servant qualified
import Utils.HTML (HTML)

--------------------------------------------------------------------------------

type Route = Servant.AuthProtect "cookie-auth" :> "user" :> "logout" :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] Servant.NoContent)

--------------------------------------------------------------------------------

handler ::
  ( Log.MonadLog m,
    MonadReader env m,
    Has OTEL.Tracer env,
    MonadDB m,
    MonadThrow m,
    MonadCatch m,
    MonadUnliftIO m
  ) =>
  Auth.Authz ->
  m
    ( Servant.Headers
        '[ Servant.Header "HX-Redirect" Text
         ]
        Servant.NoContent
    )
handler Auth.Authz {authzSession} =
  Observability.handlerSpan "GET /user/logout" () (\_ -> show ()) $ do
    Auth.expireSession (Session.dSessionId authzSession) >>= \case
      Left err -> do
        liftIO $ print err
        throwErr InternalServerError
      Right _ ->
        pure $ Servant.addHeader "/user/current" Servant.NoContent
