module API.User.Register.Post
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
import Data.Password.Argon2 (Password, hashPassword, mkPassword)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.EmailAddress (EmailAddress)
import Effects.Database.Class (MonadDB, execStatement)
import Effects.Database.Tables.User qualified as User
import GHC.Generics (Generic)
import Hasql.Interpolate (OneRow (..))
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
    :> "register"
    :> Servant.RemoteHost
    :> Servant.Header "User-Agent" Text
    :> Servant.ReqBody '[Servant.FormUrlEncoded] Register
    :> Servant.Post
         '[HTML]
         ( Servant.Headers
             '[ Servant.Header "Set-Cookie" Text,
                Servant.Header "Location" Text
              ]
             Servant.NoContent
         )

--------------------------------------------------------------------------------

data Register = Register
  { urEmail :: EmailAddress,
    urPassword :: Password
  }
  deriving stock (Generic)

instance FormUrlEncoded.FromForm Register where
  fromForm f =
    Register
      <$> FormUrlEncoded.parseUnique "email" f
      <*> fmap mkPassword (FormUrlEncoded.parseUnique "password" f)

--------------------------------------------------------------------------------

handler ::
  ( MonadReader env m,
    Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    MonadIO m,
    Has HSQL.Pool env
  ) =>
  SockAddr ->
  Maybe Text ->
  Register ->
  m
    ( Servant.Headers
        '[ Servant.Header "Set-Cookie" Text,
           Servant.Header "Location" Text
         ]
        Servant.NoContent
    )
handler sockAddr mUserAgent Register {..} = do
  execStatement (User.getUserByEmail urEmail) >>= \case
    Left err -> throwErr $ InternalServerError $ Text.pack $ show err
    Right (Just _) -> do
      Log.logInfo "Email already registered" urEmail
      pure $
        Servant.noHeader $
          Servant.addHeader "/user/register" Servant.NoContent
    Right Nothing -> do
      Log.logInfo "Registering New User" urEmail
      hashedPassword <- liftIO $ hashPassword urPassword
      execStatement (User.insertUser $ User.ModelInsert urEmail hashedPassword) >>= \case
        Left err -> throwErr $ InternalServerError $ Text.pack $ show err
        Right (OneRow uid) -> do
          Auth.login uid sockAddr mUserAgent >>= \case
            Left err ->
              throwErr $ InternalServerError $ Text.pack $ show err
            Right sessionId -> do
              pure $
                Servant.addHeader (Auth.mkCookieSession sessionId) $
                  Servant.addHeader "/" Servant.NoContent
