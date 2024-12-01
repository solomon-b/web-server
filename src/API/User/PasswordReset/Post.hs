module API.User.PasswordReset.Post where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Errors (ToServerError (..), throwErr, toErrorBody)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Has (Has)
import Data.Password.Argon2 (Password, hashPassword)
import Data.Text.Display (Display, RecordInstance (..), display)
import Deriving.Aeson qualified as Deriving
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import GHC.Generics (Generic)
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

type Route = Servant.AuthProtect "cookie-auth" :> "user" :> Servant.Capture "id" User.Id :> "password-reset" :> Servant.ReqBody '[Servant.JSON] PasswordReset :> Servant.Post '[Servant.JSON] ()

--------------------------------------------------------------------------------

data PasswordReset = PasswordReset
  { prPassword :: Password,
    prNewPassword :: Password
  }
  deriving stock (Generic)
  deriving (Display) via (RecordInstance PasswordReset)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.StripPrefix "pr", Deriving.CamelToSnake]] PasswordReset

data PasswordResetError = PasswordResetFailed

instance ToServerError PasswordResetError where
  toServerError PasswordResetFailed = Servant.err400 {Servant.errBody = toErrorBody "Password Reset Failed" 400}

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
  PasswordReset ->
  m ()
handler (Auth.Authz User.Domain {dId, dIsAdmin} _) uid PasswordReset {..} =
  Observability.handlerSpan "POST /user/:id/password-reset" () display $ do
    unless (dId == uid || dIsAdmin) (throwErr Servant.err401)
    hashedPrPassword <- liftIO $ hashPassword prPassword
    hashedPrNewPassword <- liftIO $ hashPassword prNewPassword
    execQuerySpanThrow (User.changeUserPassword uid hashedPrPassword hashedPrNewPassword) >>= \case
      Nothing -> throwErr PasswordResetFailed
      Just _ -> pure ()
