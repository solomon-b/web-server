module API.User.Login where

--------------------------------------------------------------------------------

import Auth qualified
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Has (Has)
import Data.Password.Argon2 (Password, PasswordCheck (..), checkPassword, mkPassword)
import Data.Text (Text)
import Data.Text.Display (Display (..), RecordInstance (..), display)
import Deriving.Aeson qualified as Deriving
import Domain.Types.Email
import Domain.Types.ServerSessions (ServerSession (..), serverSessionId)
import Effects.Database.Class (MonadDB)
import Effects.Database.Queries.ServerSessions (selectServerSessionByUser)
import Effects.Database.Queries.User
import Effects.Database.Tables.User (umPassword)
import Effects.Database.Tables.User qualified as User
import Errors (throw401, throw401', throw500)
import GHC.Generics (Generic)
import Log qualified
import Lucid qualified
import Network.Socket
import OpenTelemetry.Trace qualified as OTEL
import OrphanInstances ()
import Servant qualified
import Tracing (handlerSpan)
import Web.FormUrlEncoded (FromForm (..))
import Web.FormUrlEncoded qualified as FormUrlEncoded

--------------------------------------------------------------------------------

data Page = Page

instance Lucid.ToHtml Page where
  toHtml :: (Monad m) => Page -> Lucid.HtmlT m ()
  toHtml Page =
    Lucid.doctypehtml_ $ do
      Lucid.head_ $ do
        Lucid.title_ "Login"
        Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.type_ "text/css", Lucid.href_ "static/styles.css"]
        Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.type_ "text/css", Lucid.href_ "https://matcha.mizu.sh/matcha.css"]
      Lucid.body_ $ do
        Lucid.div_ $ do
          Lucid.form_ [Lucid.method_ "POST", Lucid.action_ "login"] $ do
            Lucid.input_ [Lucid.type_ "text", Lucid.name_ "email", Lucid.placeholder_ "email"]
            Lucid.input_ [Lucid.type_ "text", Lucid.name_ "password", Lucid.placeholder_ "password"]
            Lucid.button_ "submit"

  toHtmlRaw :: (Monad m) => Page -> Lucid.HtmlT m ()
  toHtmlRaw = Lucid.toHtml

--------------------------------------------------------------------------------

getHandler :: (Lucid.Html ())
getHandler = Lucid.toHtml Page

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

handler ::
  ( MonadReader env m,
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
  handlerSpan "/user/login" req display $ do
    selectUserByEmail ulEmail >>= \case
      Just user -> do
        Log.logInfo "Login Attempt" ulEmail
        unless (checkPassword ulPassword (runIdentity (umPassword user)) == PasswordCheckSuccess) throw401'
        selectServerSessionByUser (coerce $ User.umId user) >>= \case
          Nothing -> do
            Auth.login (coerce $ User.umId user) sockAddr mUserAgent >>= \case
              Left _err ->
                throw500 "Something went wrong"
              Right sessionId ->
                pure $ Servant.addHeader ("session-id=" <> display sessionId <> "; SameSite=strict") $ Servant.addHeader "/user/current" Servant.NoContent
          Just session ->
            let sessionId = serverSessionId session
             in pure $ Servant.addHeader ("session-id=" <> display sessionId <> "; SameSite=strict") $ Servant.addHeader "/user/current" Servant.NoContent
      Nothing -> do
        Log.logInfo "Invalid Credentials" ulEmail
        throw401 "Invalid Credentials."
