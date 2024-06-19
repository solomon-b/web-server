module API.User.PasswordReset where

--------------------------------------------------------------------------------

import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Has (Has)
import Data.Text.Display (Display, RecordInstance (..), display)
import Deriving.Aeson qualified as Deriving
import Domain.Types.Password (Password)
import Domain.Types.User (User (..))
import Effects.Database.Class (MonadDB)
import Effects.Database.Queries.User (changeUserPassword)
import Effects.Database.Tables.User qualified as User
import Errors (throw400, throw401')
import GHC.Generics (Generic)
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import Servant.Auth.Server qualified as SAS
import Tracing (handlerSpan)

--------------------------------------------------------------------------------

data PasswordReset = PasswordReset
  { prPassword :: Password,
    prNewPassword :: Password
  }
  deriving stock (Generic)
  deriving (Display) via (RecordInstance PasswordReset)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.StripPrefix "ur", Deriving.CamelToSnake]] PasswordReset

handler ::
  ( Log.MonadLog m,
    MonadThrow m,
    MonadReader env m,
    Has OTEL.Tracer env,
    MonadCatch m,
    MonadDB m,
    MonadUnliftIO m
  ) =>
  SAS.AuthResult User ->
  User.Id ->
  PasswordReset ->
  m ()
handler (SAS.Authenticated User {userId, userIsAdmin}) uid PasswordReset {..} =
  handlerSpan "/user/:id/password-reset" () display $ do
    unless (userId == uid || userIsAdmin) throw401'
    changeUserPassword uid prPassword prNewPassword >>= \case
      Nothing -> throw400 "Password Reset Failed"
      Just _ -> pure ()
handler _ _ _ = throw401'
