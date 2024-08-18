module API.About.Get where

--------------------------------------------------------------------------------

import Auth qualified
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Effects.Database.Class (MonadDB)
import Effects.Observability qualified as Observability
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Utils.HTML (HTML, RawHtml, toHTML)
import Widgets.About qualified as About
import Widgets.Body qualified as Body

--------------------------------------------------------------------------------

type Route = "about" :> Servant.Header "Cookie" Text :> Servant.Header "HX-Request" Bool :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "Vary" Text] RawHtml)

handler ::
  ( Has Tracer env,
    MonadCatch m,
    MonadDB m,
    MonadReader env m,
    MonadUnliftIO m
  ) =>
  Maybe Text ->
  Maybe Bool ->
  m (Servant.Headers '[Servant.Header "Vary" Text] RawHtml)
handler cookie hxTrigger =
  Observability.handlerSpan "GET /" () (const @Text "RawHtml") $ do
    loginState <- Auth.userLoginState cookie
    case hxTrigger of
      Just True ->
        pure $ Servant.addHeader "HX-Request" $ toHTML About.widget
      _ ->
        pure $ Servant.addHeader "HX-Request" $ toHTML $ Body.widget loginState About.widget
