module API.User.Login.Post where

--------------------------------------------------------------------------------

import Auth qualified
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Has (Has)
import Data.Password.Argon2 (Password, PasswordCheck (..), checkPassword, mkPassword)
import Data.Text (Text)
import Data.Text.Display (Display (..), RecordInstance (..), display)
import Deriving.Aeson qualified as Deriving
import Domain.Types.EmailAddress
import Effects.Clock (MonadClock)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.ServerSessions qualified as Session
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import Errors (InternalServerError (..), Unauthorized (..), throwErr)
import GHC.Generics (Generic)
import Log qualified
import Network.Socket
import OpenTelemetry.Trace qualified as OTEL
import OrphanInstances.Servant ()
import Servant ((:>))
import Servant qualified
import Web.FormUrlEncoded (FromForm (..))
import Web.FormUrlEncoded qualified as FormUrlEncoded

--------------------------------------------------------------------------------

type Route = "user" :> "login" :> Servant.RemoteHost :> Servant.Header "User-Agent" Text :> Servant.ReqBody '[Servant.FormUrlEncoded, Servant.JSON] Login :> Servant.Post '[Servant.JSON] (Servant.Headers '[Servant.Header "Set-Cookie" Text, Servant.Header "HX-Redirect" Text] Servant.NoContent)

--------------------------------------------------------------------------------

data Login = Login
  { ulEmail :: EmailAddress,
    ulPassword :: Password
  }
  deriving stock (Generic)
  deriving (Display) via (RecordInstance Login)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.StripPrefix "ul", Deriving.CamelToSnake]] Login

instance FormUrlEncoded.FromForm Login where
  fromForm f =
    Login
      <$> FormUrlEncoded.parseUnique "email" f
      <*> fmap mkPassword (FormUrlEncoded.parseUnique "password" f)

--------------------------------------------------------------------------------

handler ::
  ( MonadClock m,
    MonadReader env m,
    MonadIO m,
    Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    MonadUnliftIO m,
    MonadCatch m,
    Has OTEL.Tracer env
  ) =>
  SockAddr ->
  Maybe Text ->
  Login ->
  m
    ( Servant.Headers
        '[ Servant.Header "Set-Cookie" Text,
           Servant.Header "HX-Redirect" Text
         ]
        Servant.NoContent
    )
handler sockAddr mUserAgent req@Login {..} = do
  Observability.handlerSpan "POST /user/login" req display $ do
    execQuerySpanThrow (User.getUserByEmail ulEmail) >>= \case
      Just user -> do
        Log.logInfo "Login Attempt" ulEmail
        unless (checkPassword ulPassword (User.mPassword user) == PasswordCheckSuccess) (throwErr Unauthorized)
        execQuerySpanThrow (Session.getServerSessionByUser (User.mId user)) >>= \case
          Nothing -> do
            Auth.login (User.mId user) sockAddr mUserAgent >>= \case
              Left _err ->
                throwErr InternalServerError
              Right sessionId ->
                pure $ Servant.addHeader ("session-id=" <> display sessionId <> "; SameSite=strict") $ Servant.addHeader "/user/current" Servant.NoContent
          Just session ->
            let sessionId = Session.mSessionId session
             in pure $ Servant.addHeader ("session-id=" <> display sessionId <> "; SameSite=strict") $ Servant.addHeader "/user/current" Servant.NoContent
      Nothing -> do
        Log.logInfo "Invalid Credentials" ulEmail
        throwErr Unauthorized
