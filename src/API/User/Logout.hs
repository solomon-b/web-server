module API.User.Logout where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader qualified as Reader
import Data.Has (Has)
import Data.Has qualified as Has
import Data.Text.Display (display)
import Domain.Types.User (User)
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace qualified as OTEL
import Servant qualified
import Servant.Auth.Server qualified as SAS
import Tracing qualified

--------------------------------------------------------------------------------

data Page = Authenticated | NotAuthenticated

instance Lucid.ToHtml Page where
  toHtml :: (Monad m) => Page -> Lucid.HtmlT m ()
  toHtml Authenticated =
    Lucid.doctypehtml_ $ do
      Lucid.head_ $ do
        Lucid.title_ "Logout"
        Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.type_ "text/css", Lucid.href_ "https://matcha.mizu.sh/matcha.css"]
      Lucid.body_ $ do
        Lucid.div_ $ do
          Lucid.p_ "You have been logged out."
  toHtml NotAuthenticated =
    Lucid.doctypehtml_ $ do
      Lucid.head_ $ do
        Lucid.title_ "Logout"
        Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.type_ "text/css", Lucid.href_ "https://matcha.mizu.sh/matcha.css"]
      Lucid.body_ $ do
        Lucid.div_ $ do
          Lucid.p_ "You are not logged in."

  toHtmlRaw :: (Monad m) => Page -> Lucid.HtmlT m ()
  toHtmlRaw = Lucid.toHtml

--------------------------------------------------------------------------------

handler ::
  ( Log.MonadLog m,
    MonadReader env m,
    Has SAS.CookieSettings env,
    Has OTEL.Tracer env,
    MonadThrow m,
    MonadCatch m,
    MonadUnliftIO m
  ) =>
  SAS.AuthResult User ->
  m (Servant.Headers '[Servant.Header "Set-Cookie" SAS.SetCookie, Servant.Header' '[Servant.Optional, Servant.Strict] "Set-Cookie" SAS.SetCookie] (Lucid.Html ()))
handler authResult =
  Tracing.handlerSpan "/user/logout" () (const $ display ()) $ do
    cookieSettings <- Reader.asks Has.getter
    case authResult of
      SAS.Authenticated _user -> pure $ SAS.clearSession cookieSettings $ Lucid.toHtml Authenticated
      _ -> pure $ SAS.clearSession cookieSettings $ Lucid.toHtml NotAuthenticated
