{-# LANGUAGE QuasiQuotes #-}

module API.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (mailingListPostLink)
import App.Auth qualified as Auth
import Component.Frame (loadFrameWithNav)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Effects.Database.Class (MonadDB)
import Effects.Observability qualified as Observability
import Log qualified
import Lucid (alt_, aside_, button_, class_, div_, for_, form_, h3_, id_, img_, input_, label_, name_, p_, placeholder_, required_, span_, src_, type_)
import Lucid qualified
import Lucid.Extras
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Link
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route = Servant.Header "Cookie" Text :> Servant.Get '[HTML] (Lucid.Html ())

--------------------------------------------------------------------------------

mailingListSignupUrl :: Link.URI
mailingListSignupUrl = Link.linkURI mailingListPostLink

template :: Lucid.Html ()
template =
  div_ [class_ "w-full px-4"] $ do
    div_ [class_ "hero-content wow fadeInUp mx-auto max-w-[780px] text-center"] $ do
      p_ [class_ "mx-auto mb-9 max-w-[600px] text-base font-medium sm:text-lg sm:leading-[1.44]"] $ do
        img_ [src_ "static/WWW-LetShare.svg.png", alt_ "logo"]
        "Multidisciplinary Web Template Built with Your Favourite Technology - Haskell, Htmx, and Tailwind."
    aside_ [class_ "mx-auto w-1/2 p-4 my-8 bg-white border border-gray-200 rounded-lg shadow-md sm:p-6 lg:p-8"] $ do
      h3_ [class_ "mb-3 text-xl font-medium text-gray-900"] "Get more updates..."
      p_ [class_ "mb-5 text-sm font-medium text-gray-500"] "Sign up for our mailing list hear about new features, components, versions, and tools."
      form_ [hxPost_ [i|#{mailingListSignupUrl}|], hxSwap_ "outerHTML"] $ do
        div_ [class_ "flex items-end mb-3"] $ do
          div_ [class_ "flex items-center w-full max-w-md mb-3"] $ do
            div_ [class_ "relative w-full mr-3"] $ do
              label_ [for_ "member_email", class_ "hidden block mb-2 text-sm font-medium text-gray-900"] "Email address"
              input_ [id_ "member_email", class_ "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-green-500 focus:border-green-500 block w-full pl-10 p-2.5", name_ "emailAddress", placeholder_ "Your email address...", required_ "", type_ "email"]

            button_ $
              span_ [class_ "px-5 py-3 text-sm font-medium text-center text-white bg-green-700 rounded-lg cursor-pointer hover:bg-green-800 focus:ring-4 focus:ring-green-300"] "Subscribe"

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
  m (Lucid.Html ())
handler cookie =
  Observability.handlerSpan "GET /" () display $ do
    loginState <- Auth.userLoginState cookie
    loadFrameWithNav loginState "home-tab" template
