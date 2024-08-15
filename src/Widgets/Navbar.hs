module Widgets.Navbar where

--------------------------------------------------------------------------------

import Htmx.Lucid.Core qualified as Lucid.Htmx
import Lucid (HtmlT)
import Lucid.Html5
import Utils.HTML (classes_)

--------------------------------------------------------------------------------

data LoggedIn = IsLoggedIn | IsNotLoggedIn
  deriving (Eq)

logoutButton :: (Monad m) => HtmlT m ()
logoutButton =
  button_
    [ classes_ ["block", "py-2", "mx-3", "text-gray-900", "rounded", "hover:bg-gray-100", "md:hover:bg-transparent", "md:hover:text-green-700"],
      Lucid.Htmx.hxPost_ "user/logout",
      Lucid.Htmx.hxSwap_ "innerHTML" {-,
                                     Lucid.Htmx.hxTarget_ "main"-}
    ]
    "Logout"

loginButton :: (Monad m) => HtmlT m ()
loginButton =
  button_
    [ classes_ ["block", "py-2", "mx-3", "text-gray-900", "rounded", "hover:bg-gray-100", "md:hover:bg-transparent", "md:hover:text-green-700"],
      Lucid.Htmx.hxGet_ "user/login",
      Lucid.Htmx.hxSwap_ "innerHTML",
      Lucid.Htmx.hxTarget_ "main"
    ]
    "Login"

signupButton :: (Monad m) => HtmlT m ()
signupButton =
  button_
    [ classes_ ["text-white", "bg-green-700", "hover:bg-green-800", "focus:ring-4", "focus:outline-none", "focus:ring-green-300", "font-medium", "rounded-lg", "text-sm", "px-4", "py-2", "text-center"],
      Lucid.Htmx.hxGet_ "user/register",
      Lucid.Htmx.hxSwap_ "innerHTML",
      Lucid.Htmx.hxTarget_ "main"
    ]
    "Sign Up"

navBar :: (Monad m) => LoggedIn -> HtmlT m ()
navBar loggedIn =
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
        if loggedIn == IsLoggedIn then logoutButton else loginButton *> signupButton
