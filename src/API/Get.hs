{-# LANGUAGE StandaloneDeriving #-}

module API.Get where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Foldable (Foldable (..))
import Data.Has (Has)
import Data.Text (Text)
import Data.Text.Display (Display (..), ShowInstance (..))
import Effects.Observability qualified as Observability
import Htmx.Lucid.Core qualified as Lucid.Htmx
import Htmx.Lucid.Head qualified as Lucid.Htmx
import Lucid
import OpenTelemetry.Trace (Tracer)
import Servant qualified
import Utils.HTML (HTML, RawHtml (..), classes_, toHTML)

--------------------------------------------------------------------------------
-- HTML

data SplashPage = SplashPage
  deriving stock (Show)

deriving via (ShowInstance SplashPage) instance (Display SplashPage)

header :: (Monad m) => HtmlT m ()
header =
  head_ $ do
    title_ "web-server"
    script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Html ())
    Lucid.Htmx.useHtmx

navBar :: (Monad m) => HtmlT m ()
navBar =
  nav_ [classes_ ["bg-white", "border-gray-200"]] $
    div_ [classes_ ["max-w-screen-xl", "flex", "flex-wrap", "justify-between", "items-center", "mx-auto p-4"]] $ do
      div_ [classes_ ["items-center", "hidden", "w-full", "md:flex", "md:w-auto md:order-1"]] $ do
        a_ [href_ "/", classes_ ["flex", "items-center", "space-x-3", "mr-8"]] $ do
          span_ [] "🌎"
          span_ [classes_ ["self-center", "text-2xl", "font-semibold", "whitespace-nowrap"]] "Web-Server"

        ul_ [classes_ ["flex", "flex-col", "font-medium", "p-4", "md:p-0", "mt-4", "border", "border-gray-100", "rounded-lg", "bg-gray-50", "md:space-x-8", "rtl:space-x-reverse", "md:flex-row", "md:mt-0", "md:border-0", "md:bg-white"]] $ do
          li_ [] $ a_ [href_ "#", classes_ ["block", "py-2", "px-3", "md:p-0", "text-white", "bg-green-700", "rounded", "md:bg-transparent", "md:text-green-700"]] "Home"
          li_ [] $ a_ [href_ "#", classes_ ["block", "py-2", "px-3", "md:p-0", "text-gray-900", "rounded", "hover:bg-gray-100", "md:hover:bg-transparent", "md:hover:text-green-700"]] "About"
          li_ [] $ a_ [href_ "#", classes_ ["block", "py-2", "px-3", "md:p-0", "text-gray-900", "rounded", "hover:bg-gray-100", "md:hover:bg-transparent", "md:hover:text-green-700"]] "Services"
          li_ [] $ a_ [href_ "#", classes_ ["block", "py-2", "px-3", "md:p-0", "text-gray-900", "rounded", "hover:bg-gray-100", "md:hover:bg-transparent", "md:hover:text-green-700"]] "Contact"

      div_ [id_ "login-sign-up-logout", classes_ ["flex md:order-2", "space-x-3", "md:space-x-0", "rtl:space-x-reverse"]] $ do
        button_
          [ classes_ ["block", "py-2", "mx-3", "text-gray-900", "rounded", "hover:bg-gray-100", "md:hover:bg-transparent", "md:hover:text-green-700"],
            Lucid.Htmx.hxGet_ "user/login",
            Lucid.Htmx.hxSwap_ "innerHTML",
            Lucid.Htmx.hxTarget_ "main"
          ]
          "Sign In"
        button_
          [classes_ ["text-white", "bg-green-700", "hover:bg-green-800", "focus:ring-4", "focus:outline-none", "focus:ring-green-300", "font-medium", "rounded-lg", "text-sm", "px-4", "py-2", "text-center"]]
          "Sign Up"

home :: (Monad m) => HtmlT m ()
home =
  div_ [classes_ ["w-full", "px-4"]] $
    div_ [classes_ ["hero-content", "wow", "fadeInUp", "mx-auto", "max-w-[780px]", "text-center"]] $ do
      -- h1_ [class_ ["mb-6", "text-3xl", "font-bold", "leading-snug", "sm:text-4xl", "sm:leading-snug", "lg:text-5xl", "lg:leading-[1.2]"]]
      --   "Open-Source Web Template for SaaS, Startup, Apps, and More"
      p_ [classes_ ["mx-auto", "mb-9", "max-w-[600px]", "text-base", "font-medium", "sm:text-lg", "sm:leading-[1.44]"]] $ do
        Lucid.img_ [Lucid.src_ "static/WWW-LetShare.svg.png"]
        "Multidisciplinary Web Template Built with Your Favourite Technology - Haskell, Htmx, and Tailwind."

splashPage :: (Monad m) => HtmlT m ()
splashPage =
  doctypehtml_ $ do
    header
    body_ $ do
      div_ [classes_ ["container", "mx-auto"]] $ do
        navBar
        main_ [id_ "main", classes_ ["mx-4", "flex", "flex-wrap", "items-center"]] home

--------------------------------------------------------------------------------

type Route = Servant.Get '[HTML] RawHtml

handler ::
  ( Has Tracer env,
    MonadCatch m,
    MonadReader env m,
    MonadUnliftIO m
  ) =>
  m RawHtml
handler =
  Observability.handlerSpan "GET /" () (const @Text "RawHtml") $ do
    pure $ toHTML splashPage
