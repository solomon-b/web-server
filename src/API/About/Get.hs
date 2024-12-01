{-# LANGUAGE QuasiQuotes #-}

module API.About.Get where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import Control.Lens (filtered, set, traversed, (<&>))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Catch.Pure (MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.ByteString (ByteString)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Effects.Database.Class (MonadDB)
import Effects.Observability qualified as Observability
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Text.XmlHtml qualified as Xml
import Text.XmlHtml.Optics (swapInner, _a, _docContent', _elAttributes, _elChildren', _id, _main)
import Utils.HTML (HTML, RawHtml, parseFragment, readDocument, readFragment, renderFragment, renderHTML)

--------------------------------------------------------------------------------

type Route = "about" :> Servant.Header "Cookie" Text :> Servant.Header "HX-Request" Bool :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "Vary" Text] RawHtml)

--------------------------------------------------------------------------------

template :: ByteString
template =
  [i|<div class="w-2/3 mx-auto">
  <img src="static/what-is-Hypertext-Transfer-Protocol-HTTP-1920x1080.jpg" class="h-auto max-w-xl mx-auto my-3">
  <h1 class="mb-4 text-4xl text-center font-extrabold leading-none tracking-tight text-gray-900 md:text-5xl lg:text-6xl">Connect, Create, and Captivate.
  </h1>
  <h2 class="text-4xl font-bold">Overview of HyperNet
  </h2>
  <p class="mb-3 text-lg text-gray-500 md:text-xl">HyperNet is an advanced hypermedia system designed to enhance user interaction with digital content. This innovative platform allows developers to integrate text, images, audio, and video into a single, interactive environment.
  </p>
  <p class="mb-3 text-gray-500">Leveraging the power of the World Wide Web, HyperNet supports the creation of rich multimedia presentations accessible through any web browser. With its intuitive interface, content creators can easily link media elements using clickable hotspots and hyperlinks, creating a dynamic navigation experience.
  </p>
  <h2 class="text-4xl font-bold">Features and Functionality
  </h2>
  <p class="text-gray-500">HyperNet is built on a flexible architecture that supports integration with various legacy systems and media formats, making it ideal for corporate and educational settings. Its support for CGI scripting and server-side includes allows developers to create personalized and interactive content, enhancing user engagement. The system’s compatibility with popular media standards like JPEG, GIF, WAV, and MPEG ensures smooth playback and broad accessibility. HyperNet’s comprehensive documentation and active user community provide a wealth of resources for developers, fostering a collaborative environment.
  </p>
</div>|]

--------------------------------------------------------------------------------

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

    pageFragment <- parseFragment template
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
