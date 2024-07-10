module API where

--------------------------------------------------------------------------------

import API.Admin
import API.MailingList
import API.SplashPage
import API.User
import Config (Environment, Hostname, SmtpConfig)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Effects.Database.Class (MonadDB)
import Effects.Email.Class (MonadEmail)
import Hasql.Pool qualified as HSQL
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import Servant ((:<|>) (..), (:>))
import Servant qualified

--------------------------------------------------------------------------------

type API =
  SplashPageAPI
    :<|> "mailing-list" :> MailingListAPI
    :<|> "user" :> UserAPI
    :<|> Servant.AuthProtect "cookie-auth" :> "user" :> UserProtectedAPI
    :<|> "admin" :> AdminAPI

server ::
  ( MonadReader env m,
    Has HSQL.Pool env,
    Has OTEL.Tracer env,
    Has SmtpConfig env,
    Has Hostname env,
    Log.MonadLog m,
    MonadDB m,
    MonadEmail m,
    MonadIO m,
    MonadThrow m,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  Environment ->
  Servant.ServerT API m
server env =
  splashPageHandler env
    :<|> mailingListHandler
    :<|> userHandler
    :<|> userProtectedHandler
    :<|> adminPageHandler
