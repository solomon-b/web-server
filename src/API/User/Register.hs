module API.User.Register where

--------------------------------------------------------------------------------

import Auth qualified
import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Catch.Pure (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.CaseInsensitive qualified as CI
import Data.Coerce (coerce)
import Data.Has (Has)
import Data.Password.Argon2 (Password, hashPassword)
import Data.Text (Text)
import Data.Text.Display (Display, display)
import Data.Text.Display.Generic (RecordInstance (..))
import Data.Text.Encoding qualified as Text.Encoding
import Deriving.Aeson qualified as Deriving
import Domain.Types.AdminStatus
import Domain.Types.DisplayName
import Domain.Types.Email
import Effects.Database.Class (MonadDB)
import Effects.Database.Queries.User
import Effects.Database.Utils
import Errors (throw401, throw500, throw500')
import GHC.Generics (Generic)
import Log qualified
import Network.Socket (SockAddr)
import OpenTelemetry.Trace qualified as OTEL
import Servant qualified
import Text.Email.Validate qualified as Email
import Tracing (handlerSpan)

--------------------------------------------------------------------------------

data Register = Register
  { urEmail :: EmailAddress,
    urPassword :: Password,
    urDisplayName :: DisplayName
  }
  deriving stock (Generic)
  deriving (Display) via (RecordInstance Register)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.StripPrefix "ur", Deriving.CamelToSnake]] Register

handler ::
  ( MonadReader env m,
    Has OTEL.Tracer env,
    Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  SockAddr ->
  Maybe Text ->
  Register ->
  m
    ( Servant.Headers
        '[ Servant.Header "Set-Cookie" Text,
           Servant.Header "HX-Redirect" Text
         ]
        Servant.NoContent
    )
handler sockAddr mUserAgent req@Register {..} = do
  handlerSpan "/user/register" req display $ do
    unless (Email.isValid $ Text.Encoding.encodeUtf8 $ CI.original $ coerce urEmail) $ throw401 "Invalid Email Address"
    selectUserByEmail urEmail >>= \case
      Just _ -> do
        Log.logInfo "Email address is already registered" urEmail
        throw401 "Email address is already registered"
      Nothing -> do
        Log.logInfo "Registering New User" urEmail
        hashedPassword <- hashPassword urPassword
        uid <- insertUser (urEmail, hashedPassword, urDisplayName, IsNotAdmin)
        execQuerySpanThrowMessage "Failed to query users table" (selectUserQuery uid) >>= \case
          Nothing ->
            throw500'
          Just _user -> do
            Auth.login uid sockAddr mUserAgent >>= \case
              Left _err ->
                throw500 "Something went wrong"
              Right sessionId ->
                pure $ Servant.addHeader ("session-id=" <> display sessionId <> "; SameSite=strict") $ Servant.addHeader "/" Servant.NoContent
