module API where

--------------------------------------------------------------------------------

import API.Admin qualified as Admin
import API.MailingList.Post (MailingListAPI)
import API.MailingList.Post qualified as MailingList.Post
import API.SplashPage.Get (SplashPageAPI)
import API.SplashPage.Get qualified as SplashPage.Get
import API.User qualified as User
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
    :<|> "user" :> User.API
    :<|> Servant.AuthProtect "cookie-auth" :> "user" :> User.ProtectedAPI
    :<|> Servant.AuthProtect "cookie-auth" :> "admin" :> Admin.ProtectedAPI

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
  SplashPage.Get.handler env
    :<|> MailingList.Post.handler
    :<|> User.handler
    :<|> User.protectedHandler
    :<|> Admin.handler
