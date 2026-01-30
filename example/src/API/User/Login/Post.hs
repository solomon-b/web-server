module API.User.Login.Post
  ( Route,
    handler,
  )
where

import App.Auth qualified as Auth
import App.Errors (InternalServerError (..), throwErr)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Maybe (fromMaybe)
import Data.Password.Argon2 (Password, PasswordCheck (..), checkPassword, mkPassword)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.EmailAddress (EmailAddress)
import Effects.Database.Class (MonadDB, execStatement)
import Effects.Database.Tables.ServerSessions qualified as Session
import Effects.Database.Tables.User qualified as User
import GHC.Generics (Generic)
import Hasql.Pool qualified as HSQL
import Log qualified
import Network.Socket (SockAddr)
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Web.FormUrlEncoded qualified as FormUrlEncoded

--------------------------------------------------------------------------------

type Route =
  "user"
    :> "login"
    :> Servant.RemoteHost
    :> Servant.Header "User-Agent" Text
    :> Servant.ReqBody '[Servant.FormUrlEncoded] Login
    :> Servant.QueryParam "redirect" Text
    :> Servant.Post
         '[HTML]
         ( Servant.Headers
             '[ Servant.Header "Set-Cookie" Text,
                Servant.Header "Location" Text
              ]
             Servant.NoContent
         )

--------------------------------------------------------------------------------

data Login = Login
  { ulEmail :: EmailAddress,
    ulPassword :: Password
  }
  deriving stock (Generic)

instance FormUrlEncoded.FromForm Login where
  fromForm f =
    Login
      <$> FormUrlEncoded.parseUnique "email" f
      <*> fmap mkPassword (FormUrlEncoded.parseUnique "password" f)

--------------------------------------------------------------------------------

handler ::
  ( MonadReader env m,
    MonadIO m,
    Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    Has HSQL.Pool env
  ) =>
  SockAddr ->
  Maybe Text ->
  Login ->
  Maybe Text ->
  m
    ( Servant.Headers
        '[ Servant.Header "Set-Cookie" Text,
           Servant.Header "Location" Text
         ]
        Servant.NoContent
    )
handler sockAddr mUserAgent Login {..} redirectQueryParam = do
  execStatement (User.getUserByEmail ulEmail) >>= \case
    Left err -> throwErr $ InternalServerError $ Text.pack $ show err
    Right Nothing -> invalidCredentialResponse ulEmail
    Right (Just user) -> do
      Log.logInfo "Login Attempt" ulEmail
      if checkPassword ulPassword (User.mPassword user) == PasswordCheckSuccess
        then do
          let redirectLink = fromMaybe "/" redirectQueryParam
          attemptLogin sockAddr mUserAgent redirectLink user
        else invalidCredentialResponse ulEmail

attemptLogin ::
  ( MonadReader env m,
    MonadThrow m,
    MonadDB m,
    Log.MonadLog m,
    Has HSQL.Pool env
  ) =>
  SockAddr ->
  Maybe Text ->
  Text ->
  User.Model ->
  m
    ( Servant.Headers
        '[ Servant.Header "Set-Cookie" Text,
           Servant.Header "Location" Text
         ]
        Servant.NoContent
    )
attemptLogin sockAddr mUserAgent redirectLink user = do
  execStatement (Session.getServerSessionByUser (User.mId user)) >>= \case
    Left err -> throwErr $ InternalServerError $ Text.pack $ show err
    Right Nothing -> do
      Auth.login (User.mId user) sockAddr mUserAgent >>= \case
        Left err ->
          throwErr $ InternalServerError $ Text.pack $ show err
        Right sessionId -> do
          pure $
            Servant.addHeader (Auth.mkCookieSession sessionId) $
              Servant.addHeader redirectLink Servant.NoContent
    Right (Just session) ->
      let sessionId = Session.mSessionId session
       in pure $
            Servant.addHeader (Auth.mkCookieSession sessionId) $
              Servant.addHeader redirectLink Servant.NoContent

invalidCredentialResponse ::
  (Log.MonadLog m) =>
  EmailAddress ->
  m
    ( Servant.Headers
        '[ Servant.Header "Set-Cookie" Text,
           Servant.Header "Location" Text
         ]
        Servant.NoContent
    )
invalidCredentialResponse emailAddress = do
  Log.logInfo "Invalid Credentials" emailAddress
  pure $
    Servant.noHeader $
      Servant.addHeader "/user/login" Servant.NoContent
