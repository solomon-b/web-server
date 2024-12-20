{-# LANGUAGE QuasiQuotes #-}

module API.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (mailingListPostLink)
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
import Log qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Link
import Text.HTML (HTML, RawHtml (..), parseFragment, renderDocument)

--------------------------------------------------------------------------------

type Route = Servant.Header "Cookie" Text :> Servant.Get '[HTML] RawHtml

--------------------------------------------------------------------------------

mailingListSignupUrl :: Link.URI
mailingListSignupUrl = Link.linkURI mailingListPostLink

template :: ByteString
template =
  [i|<div class="w-full px-4">
  <div class="hero-content wow fadeInUp mx-auto max-w-[780px] text-center">
    <p class="mx-auto mb-9 max-w-[600px] text-base font-medium sm:text-lg sm:leading-[1.44]">
      <img src="static/WWW-LetShare.svg.png" alt="logo">Multidisciplinary Web Template Built with Your Favourite Technology - Haskell, Htmx, and Tailwind.
    </p>
  </div>
  <aside class="mx-auto w-1/2 p-4 my-8 bg-white border border-gray-200 rounded-lg shadow-md sm:p-6 lg:p-8">
    <h3 class="mb-3 text-xl font-medium text-gray-900">Get more updates...
    </h3>
    <p class="mb-5 text-sm font-medium text-gray-500">Sign up for our mailing list hear about new features, components, versions, and tools.
    </p>
    <form hx-post="#{mailingListSignupUrl}" hx-swap="outerHTML">
      <div class="flex items-end mb-3">
        <div class="flex items-center w-full max-w-md mb-3">
          <div class="relative w-full mr-3">
            <label for="member_email" class="hidden block mb-2 text-sm font-medium text-gray-900">Email address
            </label>
            <input id="member_email" class="bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-green-500 focus:border-green-500 block w-full pl-10 p-2.5" name="emailAddress" placeholder="Your email address..." required="" type="email">
          </div>
          <button>
            <span class="px-5 py-3 text-sm font-medium text-center text-white bg-green-700 rounded-lg cursor-pointer hover:bg-green-800 focus:ring-4 focus:ring-green-300">Subscribe
            </span>
          </button>
        </div>
      </div>
    </form>
  </aside>
</div>
|]

--------------------------------------------------------------------------------

handler ::
  ( Has Tracer env,
    MonadCatch m,
    MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m
  ) =>
  Maybe Text ->
  m RawHtml
handler cookie =
  Observability.handlerSpan "GET /" () display $ do
    loginState <- Auth.userLoginState cookie

    pageFragment <- parseFragment template
    page <- loadFrameWithNav loginState "home-tab" pageFragment

    pure $ renderDocument page
