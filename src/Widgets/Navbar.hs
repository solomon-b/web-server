module Widgets.Navbar where

--------------------------------------------------------------------------------

import Auth (LoggedIn (..))
import Htmx.Lucid.Core qualified as Htmx
import Lucid (HtmlT)
import Lucid.Html5
import Utils.HTML (classes_)

--------------------------------------------------------------------------------

searchButton :: (Monad m) => HtmlT m ()
searchButton =
  button_
    [ classes_ ["py-2", "text-gray-900", "rounded"],
      Htmx.hxGet_ "/search",
      Htmx.hxSwap_ "innerHTML",
      Htmx.hxTarget_ "main",
      Htmx.hxPushUrl_ "true"
    ]
    $ span_ [] "🔎"

searchForm :: (Monad m) => HtmlT m ()
searchForm =
  form_ [classes_ ["max-w-md mx-auto"]] $ do
    label_ [for_ "default-search", classes_ ["mb-2", "text-sm", "font-medium", "text-gray-900", "sr-only"]] "Search"
    div_ [class_ "relative"] $ do
      div_ [classes_ ["absolute", "inset-y-0", "start-0", "flex", "items-center", "ps-3", "pointer-events-none"]] $
        span_ [] "🔎"
      input_ [type_ "search", id_ "default-search", classes_ ["block", "w-full", "p-2", "ps-10", "text-sm", "text-gray-900", "border", "border-gray-300", "rounded-lg", "bg-gray-50", "focus:ring-green-500", "focus:border-green-500"], placeholder_ "Search...", required_ "true"]

logoutButton :: (Monad m) => HtmlT m ()
logoutButton =
  button_
    [ classes_ ["py-2", "text-gray-900", "rounded", "hover:bg-gray-100", "md:hover:bg-transparent", "md:hover:text-green-700"],
      Htmx.hxPost_ "/user/logout",
      Htmx.hxSwap_ "innerHTML",
      Htmx.hxPushUrl_ "true"
    ]
    "Logout"

loginButton :: (Monad m) => HtmlT m ()
loginButton =
  button_
    [ classes_ ["py-2", "text-gray-900", "rounded", "hover:bg-gray-100", "md:hover:bg-transparent", "md:hover:text-green-700"],
      Htmx.hxGet_ "/user/login",
      Htmx.hxSwap_ "innerHTML",
      Htmx.hxTarget_ "main",
      Htmx.hxPushUrl_ "true"
    ]
    "Login"

signupButton :: (Monad m) => HtmlT m ()
signupButton =
  button_
    [ classes_ ["text-white", "bg-green-700", "hover:bg-green-800", "focus:ring-4", "focus:outline-none", "focus:ring-green-300", "font-medium", "rounded-lg", "text-sm", "px-4", "py-2", "text-center"],
      Htmx.hxGet_ "/user/register",
      Htmx.hxSwap_ "innerHTML",
      Htmx.hxTarget_ "main",
      Htmx.hxPushUrl_ "true"
    ]
    "Sign Up"

tabs :: (Monad m) => HtmlT m ()
tabs =
  ul_ [classes_ ["flex", "flex-col", "font-medium", "p-4", "md:p-0", "mt-4", "border", "border-gray-100", "rounded-lg", "bg-gray-50", "md:space-x-8", "rtl:space-x-reverse", "md:flex-row", "md:mt-0", "md:border-0", "md:bg-white"]] $ do
    li_ [] $
      a_
        [ href_ "/",
          classes_ ["block", "py-2", "px-3", "md:p-0", "text-white", "bg-green-700", "rounded", "md:bg-transparent", "md:text-green-700"],
          Htmx.hxGet_ "/",
          Htmx.hxSwap_ "innerHTML",
          Htmx.hxTarget_ "main",
          Htmx.hxPushUrl_ "true"
        ]
        "Home"
    li_ [] $
      a_
        [ href_ "#",
          classes_ ["block", "py-2", "px-3", "md:p-0", "text-gray-900", "rounded", "hover:bg-gray-100", "md:hover:bg-transparent", "md:hover:text-green-700"]
        ]
        "Blog"
    li_ [] $
      a_
        [ href_ "/about",
          classes_ ["block", "py-2", "px-3", "md:p-0", "text-gray-900", "rounded", "hover:bg-gray-100", "md:hover:bg-transparent", "md:hover:text-green-700"],
          Htmx.hxGet_ "/about",
          Htmx.hxSwap_ "innerHTML",
          Htmx.hxTarget_ "main",
          Htmx.hxPushUrl_ "true"
        ]
        "About"
    li_ [] $
      a_
        [ href_ "#",
          classes_ ["block", "py-2", "px-3", "md:p-0", "text-gray-900", "rounded", "hover:bg-gray-100", "md:hover:bg-transparent", "md:hover:text-green-700"]
        ]
        "Contact"

navBar :: (Monad m) => LoggedIn -> HtmlT m ()
navBar loggedIn =
  nav_ [classes_ ["bg-white", "mb-2"]] $
    div_ [classes_ ["max-w-screen-2xl", "flex", "flex-wrap", "justify-between", "items-center", "mx-auto p-4"]] $ do
      div_ [classes_ ["items-center", "hidden", "w-full", "md:flex", "md:w-auto md:order-1"]] $ do
        a_
          [ href_ "/",
            classes_ ["flex", "items-center", "space-x-3", "mr-8"],
            Htmx.hxGet_ "/",
            Htmx.hxSwap_ "innerHTML",
            Htmx.hxTarget_ "main",
            Htmx.hxPushUrl_ "true"
          ]
          $ do
            span_ [] "🌎"
            span_ [classes_ ["self-center", "text-2xl", "font-semibold", "whitespace-nowrap"]] "HyperNet"

        tabs
      div_ [id_ "right-nav", classes_ ["flex md:order-2", "space-x-3", "rtl:space-x-reverse"]] $ do
        searchForm
        case loggedIn of
          IsLoggedIn -> logoutButton
          IsNotLoggedIn -> loginButton *> signupButton
