module API.About.Get where

--------------------------------------------------------------------------------

import Auth qualified
import Control.Lens (filtered, set, traversed, (<&>))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Catch.Pure (MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Effects.Database.Class (MonadDB)
import Effects.Observability qualified as Observability
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Text.XmlHtml qualified as Xml
import Text.XmlHtml.Optics (swapInner, _a, _docContent', _elAttributes, _elChildren', _id, _main)
import Utils.HTML (HTML, RawHtml, readDocument, readFragment, renderFragment, renderHTML)

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

    pageFragment <- readFragment "src/Templates/Root/About/get.html"
    authFragment <- readUserAuthFragment loginState

    template <- readDocument "src/Templates/index.html" <&> updateTabHighlight . updateAuthLinks authFragment

    case hxTrigger of
      Just True ->
        pure $ Servant.addHeader "HX-Request" $ renderFragment pageFragment
      _ -> do
        let html = renderHTML $ swapMain pageFragment template
        pure $ Servant.addHeader "HX-Request" html

--------------------------------------------------------------------------------

updateTabHighlight :: Xml.Document -> Xml.Document
updateTabHighlight =
  set (_docContent' . _id "about-tab" . _elChildren' . _a . _elAttributes . traversed . filtered (\(k, _) -> k == "class")) ("class", focused)
  where
    focused = "block py-2 px-3 text-white bg-green-700 rounded md:bg-transparent md:text-green-700 md:p-0"

updateAuthLinks :: [Xml.Node] -> Xml.Document -> Xml.Document
updateAuthLinks = swapInner (_id "user-auth-links")

swapMain :: [Xml.Node] -> Xml.Document -> Xml.Document
swapMain = swapInner _main

readUserAuthFragment :: (MonadIO m, MonadThrow m) => Auth.LoggedIn -> m [Xml.Node]
readUserAuthFragment = \case
  Auth.IsLoggedIn -> readFragment "src/Templates/Root/Logout/button.html"
  Auth.IsNotLoggedIn -> liftA2 (<>) (readFragment "src/Templates/Root/Login/button.html") (readFragment "src/Templates/Root/Register/button.html")
