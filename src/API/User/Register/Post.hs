module API.User.Register.Post where

--------------------------------------------------------------------------------

import Auth qualified
import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Catch.Pure (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Has (Has)
import Data.Password.Argon2 (Password, hashPassword, mkPassword)
import Data.Text (Text)
import Data.Text.Display (Display, display)
import Data.Text.Display.Generic (RecordInstance (..))
import Deriving.Aeson qualified as Deriving
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress, isValid)
import Effects.Clock (MonadClock)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import Errors (Forbidden (..), InternalServerError (..), ToServerError (..), Unauthorized (..), throwErr)
import GHC.Generics (Generic)
import Hasql.Interpolate (OneRow (..))
import Log qualified
import Network.Socket (SockAddr)
import OpenTelemetry.Trace qualified as OTEL
import OrphanInstances.OneRow ()
import OrphanInstances.Servant ()
import Servant ((:>))
import Servant qualified
import Utils.HTML (HTML)
import Web.FormUrlEncoded qualified as FormUrlEncoded

--------------------------------------------------------------------------------

type Route =
  "user"
    :> "register"
    :> Servant.RemoteHost
    :> Servant.Header "User-Agent" Text
    :> Servant.ReqBody '[Servant.FormUrlEncoded] Register
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "Set-Cookie" Text, Servant.Header "HX-Redirect" Text] Servant.NoContent)

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

instance FormUrlEncoded.FromForm Register where
  fromForm f =
    Register
      <$> FormUrlEncoded.parseUnique "email" f
      <*> fmap mkPassword (FormUrlEncoded.parseUnique "password" f)
      <*> FormUrlEncoded.parseUnique "displayName" f

--------------------------------------------------------------------------------

data RegisterError = AlreadyRegistered

instance ToServerError RegisterError where
  toServerError :: RegisterError -> Servant.ServerError
  toServerError AlreadyRegistered = Servant.err401 {Servant.errBody = "Email address is already registered"}

--------------------------------------------------------------------------------

handler ::
  ( MonadClock m,
    MonadReader env m,
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
  Observability.handlerSpan "POST /user/register" req display $ do
    unless (isValid urEmail) $ throwErr Unauthorized
    execQuerySpanThrow (User.getUserByEmail urEmail) >>= \case
      Just _ -> do
        Log.logInfo "Email address is already registered" urEmail
        throwErr AlreadyRegistered
      Nothing -> do
        Log.logInfo "Registering New User" urEmail
        hashedPassword <- hashPassword urPassword
        OneRow uid <- execQuerySpanThrow $ User.insertUser $ User.ModelInsert urEmail hashedPassword urDisplayName Nothing False
        execQuerySpanThrow (User.getUser uid) >>= \case
          Nothing ->
            throwErr Forbidden
          Just _user -> do
            Auth.login uid sockAddr mUserAgent >>= \case
              Left _err ->
                throwErr InternalServerError
              Right sessionId -> do
                pure $ Servant.addHeader (Auth.mkCookieSession sessionId) $ Servant.addHeader "/" Servant.NoContent
