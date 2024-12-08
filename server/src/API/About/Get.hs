{-# LANGUAGE QuasiQuotes #-}

module API.About.Get where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import Component.Frame (loadFrameWithNav)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.ByteString (ByteString)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Effects.Database.Class (MonadDB)
import Effects.Observability qualified as Observability
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML, RawHtml, parseFragment, renderDocument, renderNodes)
import Text.XmlHtml.Optics (swapInner, _main)

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
  Observability.handlerSpan "GET /" () (display . Servant.getResponse) $ do
    loginState <- Auth.userLoginState cookie

    pageFragment <- parseFragment template
    page <- loadFrameWithNav loginState "about-tab" pageFragment

    case hxTrigger of
      Just True ->
        pure $ Servant.addHeader "HX-Request" $ renderNodes pageFragment
      _ -> do
        let html = renderDocument $ swapInner _main pageFragment page
        pure $ Servant.addHeader "HX-Request" html
