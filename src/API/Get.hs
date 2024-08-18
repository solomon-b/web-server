module API.Get where

--------------------------------------------------------------------------------

import Auth qualified
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Effects.Database.Class (MonadDB)
import Effects.Observability qualified as Observability
import Lucid
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Utils.HTML (HTML, RawHtml (..), classes_, toHTML)
import Widgets.Body qualified as Body
import Widgets.Navbar qualified as Navbar

--------------------------------------------------------------------------------

page :: (Monad m) => Navbar.LoggedIn -> HtmlT m ()
page loggedIn =
  Body.widget loggedIn $ do
    div_ [classes_ ["w-full", "px-4"]] $
      div_ [classes_ ["hero-content", "wow", "fadeInUp", "mx-auto", "max-w-[780px]", "text-center"]] $ do
        p_ [classes_ ["mx-auto", "mb-9", "max-w-[600px]", "text-base", "font-medium", "sm:text-lg", "sm:leading-[1.44]"]] $ do
          Lucid.img_ [Lucid.src_ "static/WWW-LetShare.svg.png"]
          "Multidisciplinary Web Template Built with Your Favourite Technology - Haskell, Htmx, and Tailwind."

--------------------------------------------------------------------------------

type Route = Servant.Header "Cookie" Text :> Servant.Get '[HTML] RawHtml

handler ::
  ( Has Tracer env,
    MonadCatch m,
    MonadDB m,
    MonadReader env m,
    MonadUnliftIO m
  ) =>
  Maybe Text ->
  m RawHtml
handler cookie =
  Observability.handlerSpan "GET /" () (const @Text "RawHtml") $ do
    loginState <- Auth.userLoginState cookie
    pure $ toHTML $ page loginState
