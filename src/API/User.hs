module API.User where

--------------------------------------------------------------------------------

import API.User.Current.Get qualified as Current.Get
import API.User.Delete qualified as Delete
import API.User.Login.Get qualified as Login.Get
import API.User.Login.Post (Login)
import API.User.Login.Post qualified as Login.Post
import API.User.Logout.Get qualified as Logout.Get
import API.User.PasswordReset.Post (PasswordReset)
import API.User.PasswordReset.Post qualified as PasswordReset.Post
import API.User.Register.Post (Register)
import API.User.Register.Post qualified as Register.Post
import Auth qualified
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Domain.Types.User
import Effects.Database.Class (MonadDB)
import Effects.Database.Queries.User
import Effects.Database.Tables.User qualified as User
import Errors (throw403')
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace qualified as OTEL
import Servant ((:<|>) (..), (:>))
import Servant qualified
import Servant.HTML.Lucid qualified as Lucid

--------------------------------------------------------------------------------
-- Route

type API =
  Servant.Get '[Servant.JSON] [User]
    :<|> Servant.Capture "id" User.Id :> Servant.Get '[Servant.JSON] User
    :<|> "register" :> Servant.RemoteHost :> Servant.Header "User-Agent" Text :> Servant.ReqBody '[Servant.JSON] Register :> Servant.Post '[Servant.JSON] (Servant.Headers '[Servant.Header "Set-Cookie" Text, Servant.Header "HX-Redirect" Text] Servant.NoContent)
    :<|> "login" :> Servant.RemoteHost :> Servant.Header "User-Agent" Text :> Servant.ReqBody '[Servant.FormUrlEncoded, Servant.JSON] Login :> Servant.Post '[Servant.JSON] (Servant.Headers '[Servant.Header "Set-Cookie" Text, Servant.Header "HX-Redirect" Text] Servant.NoContent)
    :<|> "login" :> Servant.Get '[Lucid.HTML] (Lucid.Html ())

type ProtectedAPI =
  "current" :> Servant.Get '[Servant.JSON] User
    :<|> "logout" :> Servant.Get '[Lucid.HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] Servant.NoContent)
    :<|> Servant.Capture "id" User.Id :> "delete" :> Servant.Delete '[Servant.JSON] ()
    :<|> Servant.Capture "id" User.Id :> "password-reset" :> Servant.ReqBody '[Servant.JSON] PasswordReset :> Servant.Post '[Servant.JSON] ()

--------------------------------------------------------------------------------
-- Handler

handler ::
  ( MonadReader env m,
    Has OTEL.Tracer env,
    Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  Servant.ServerT API m
handler = usersHandler :<|> userProfileHandler :<|> Register.Post.handler :<|> Login.Post.handler :<|> Login.Get.handler

protectedHandler ::
  ( MonadReader env m,
    Has OTEL.Tracer env,
    Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  Auth.Authz ->
  Servant.ServerT ProtectedAPI m
protectedHandler authz = Current.Get.handler authz :<|> Logout.Get.handler authz :<|> Delete.handler authz :<|> PasswordReset.Post.handler authz

usersHandler ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m
  ) =>
  m [User]
usersHandler = selectUsers

userProfileHandler ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m
  ) =>
  User.Id ->
  m User
userProfileHandler uid =
  selectUser uid >>= \case
    Nothing -> throw403'
    Just user -> pure user
