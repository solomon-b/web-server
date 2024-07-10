module API.User.Logout where

--------------------------------------------------------------------------------

import Auth qualified
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadIO (..), MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Domain.Types.ServerSessions qualified as ServerSessions
import Effects.Database.Class (MonadDB)
import Errors (throw500)
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace qualified as OTEL
import Servant qualified
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
    Has OTEL.Tracer env,
    MonadDB m,
    MonadThrow m,
    MonadCatch m,
    MonadUnliftIO m
  ) =>
  Auth.Authz ->
  m
    ( Servant.Headers
        '[ Servant.Header "HX-Redirect" Text
         ]
        Servant.NoContent
    )
handler Auth.Authz {authzSession} =
  Tracing.handlerSpan "/user/logout" () (\_ -> show ()) $ do
    Auth.expireSession (ServerSessions.serverSessionId authzSession) >>= \case
      Left _ ->
        throw500 "Something went wrong"
      Right _ ->
        pure $ Servant.addHeader "/user/current" Servant.NoContent
