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
import Htmx.Lucid.Core qualified as Htmx.Lucid
import Lucid
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Utils.HTML (HTML, RawHtml (..), classes_, toHTML)
import Widgets.Body qualified as Body

--------------------------------------------------------------------------------

page :: (Monad m) => Auth.LoggedIn -> HtmlT m ()
page loggedIn =
  Body.widget loggedIn $ do
    div_ [classes_ ["w-full", "px-4"]] $ do
      div_ [classes_ ["hero-content", "wow", "fadeInUp", "mx-auto", "max-w-[780px]", "text-center"]] $ do
        p_ [classes_ ["mx-auto", "mb-9", "max-w-[600px]", "text-base", "font-medium", "sm:text-lg", "sm:leading-[1.44]"]] $ do
          Lucid.img_ [Lucid.src_ "static/WWW-LetShare.svg.png"]
          "Multidisciplinary Web Template Built with Your Favourite Technology - Haskell, Htmx, and Tailwind."

      aside_ [classes_ ["mx-auto", "w-1/2", "p-4 my-8 bg-white border border-gray-200 rounded-lg shadow-md sm:p-6 lg:p-8 dark:bg-gray-800 dark:border-gray-700"], label_ "Subscribe to the Flowbite newsletter"] $ do
        h3_ [classes_ ["mb-3 text-xl font-medium text-gray-900 dark:text-white"]] "Get more updates..."
        p_
          [classes_ ["mb-5 text-sm font-medium text-gray-500 dark:text-gray-300"]]
          "Sign up for our mailing list hear about new features, components, versions, and tools."

        form_ [Htmx.Lucid.hxPost_ "mailing-list/signup", Htmx.Lucid.hxSwap_ "outerHTML", classes_ ["seva-form formkit-form"]] $ do
          div_ [classes_ ["flex items-end mb-3"]] $ do
            div_ [classes_ ["flex items-center w-full max-w-md mb-3 seva-fields formkit-fields"]] $ do
              div_ [classes_ ["relative w-full mr-3 formkit-field"]] $ do
                label_ [for_ "member_email", classes_ ["hidden block mb-2 text-sm font-medium text-gray-900 dark:text-gray-300"]] "Email address"
                input_ [classes_ ["formkit-input bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-green-500 focus:border-green-500 block w-full pl-10 p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-green-500 dark:focus:border-green-500"], name_ "emailAddress", label_ "Email Address", placeholder_ "Your email address...", required_ "", type_ "email"]
              button_ [classes_ ["formkit-submit"]] $ do
                span_ [classes_ ["px-5 py-3 text-sm font-medium text-center text-white bg-green-700 rounded-lg cursor-pointer hover:bg-green-800 focus:ring-4 focus:ring-green-300 dark:bg-green-600 dark:hover:bg-green-700 dark:focus:ring-green-800"]] "Subscribe"

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
