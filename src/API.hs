module API where

--------------------------------------------------------------------------------

import API.Admin.Get qualified as Admin.Get
import API.Get qualified as Get
import API.MailingList.Post qualified as MailingList.Post
import API.Static.Get qualified as Static.Get
import API.User.Current.Get qualified as User.Current.Get
import API.User.Delete qualified as Delete
import API.User.Delete qualified as User.Delete
import API.User.Get qualified as User.Get
import API.User.Id.Get qualified as User.Id.Get
import API.User.Login.Get qualified as User.Login.Get
import API.User.Login.Post qualified as User.Login.Post
import API.User.Logout.Get qualified as Logout.Get
import API.User.Logout.Get qualified as User.Logout.Get
import API.User.PasswordReset.Post qualified as PasswordReset.Post
import API.User.PasswordReset.Post qualified as User.PasswordReset.Post
import API.User.Register.Post qualified as User.Register.Post
import Config (Environment, Hostname, SmtpConfig)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Effects.Clock (MonadClock)
import Effects.Database.Class (MonadDB)
import Effects.MailSender (MonadEmail)
import Hasql.Pool qualified as HSQL
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import OpenTelemetry.Trace.Monad (MonadTracer)
import Servant ((:<|>) (..))
import Servant qualified

--------------------------------------------------------------------------------

type API =
  -- Unprotected homepage Routes
  Get.Route
    :<|> Static.Get.Route
    :<|> MailingList.Post.Route
    -- Unprotected User Routes
    :<|> User.Get.Route
    :<|> User.Id.Get.Route
    :<|> User.Register.Post.Route
    :<|> User.Login.Post.Route
    :<|> User.Login.Get.Route
    -- Protected User Routes
    :<|> User.Current.Get.Route
    :<|> User.Logout.Get.Route
    :<|> User.Delete.Route
    :<|> User.PasswordReset.Post.Route
    -- Protected Admin Routes
    :<|> Admin.Get.Route

server ::
  ( MonadReader env m,
    Has HSQL.Pool env,
    Has OTEL.Tracer env,
    Has SmtpConfig env,
    Has Hostname env,
    Has Environment env,
    Log.MonadLog m,
    MonadClock m,
    MonadDB m,
    MonadEmail m,
    MonadIO m,
    MonadTracer m,
    MonadThrow m,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  Environment ->
  Servant.ServerT API m
server env = do
  Get.handler
    :<|> Static.Get.handler env
    :<|> MailingList.Post.handler
    :<|> User.Get.handler
    :<|> User.Id.Get.handler
    :<|> User.Register.Post.handler
    :<|> User.Login.Post.handler
    :<|> User.Login.Get.handler
    :<|> User.Current.Get.handler
    :<|> Logout.Get.handler
    :<|> Delete.handler
    :<|> PasswordReset.Post.handler
    :<|> Admin.Get.handler
