module Widgets.RegisterForm where

--------------------------------------------------------------------------------

import Htmx.Lucid.Core (hxPost_)
import Lucid (HtmlT)
import Lucid.Html5
import Utils.HTML (classes_)

--------------------------------------------------------------------------------

widget :: (Monad m) => HtmlT m ()
widget =
  div_ [id_ "authentication-modal", classes_ ["overflow-y-auto overflow-x-hidden block z-50 justify-center items-center w-full md:inset-0 h-[calc(100%-1rem)] max-h-full"]] $
    div_ [classes_ ["relative p-4 w-full max-w-md max-h-full mx-auto"]] $
      div_ [classes_ ["relative bg-white rounded-lg shadow dark:bg-gray-700"]] $ do
        div_ [classes_ ["flex items-center justify-between p-4 md:p-5 border-b rounded-t dark:border-gray-600"]] $ do
          h3_ [classes_ ["text-xl font-semibold text-gray-900 dark:text-white"]] "Sign in"
        div_ [classes_ ["p-4 md:p-5"]] $
          form_ [hxPost_ "/user/register", class_ "space-y-4"] $ do
            div_ [] $ do
              label_ [for_ "displayName", classes_ ["block mb-2 text-sm font-medium text-gray-900 dark:text-white"]] "Your name"
              input_ [type_ "displayName", name_ "displayName", id_ "displayName", classes_ ["bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-green-500 focus:border-green-500 block w-full p-2.5 dark:bg-gray-600 dark:border-gray-500 dark:placeholder-gray-400 dark:text-white"], placeholder_ "Jason Bourne"]
            div_ [] $ do
              label_ [for_ "email", classes_ ["block mb-2 text-sm font-medium text-gray-900 dark:text-white"]] "Your email"
              input_ [type_ "email", name_ "email", id_ "email", classes_ ["bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-green-500 focus:border-green-500 block w-full p-2.5 dark:bg-gray-600 dark:border-gray-500 dark:placeholder-gray-400 dark:text-white"], placeholder_ "name@company.com"]
            div_ [] $ do
              label_ [for_ "password", classes_ ["block mb-2 text-sm font-medium text-gray-900 dark:text-white"]] "Your password"
              input_ [type_ "password", name_ "password", id_ "password", placeholder_ "••••••••", classes_ ["bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-green-500 focus:border-green-500 block w-full p-2.5 dark:bg-gray-600 dark:border-gray-500 dark:placeholder-gray-400 dark:text-white"]]
            div_ [classes_ ["flex justify-between"]] $ do
              div_ [class_ "flex items-start"] $ do
                div_ [classes_ ["flex items-center h-5"]] $
                  input_ [id_ "remember", type_ "checkbox", value_ "", classes_ ["w-4 h-4 border border-gray-300 rounded bg-gray-50 focus:ring-3 focus:ring-green-300 dark:bg-gray-600 dark:border-gray-500 dark:focus:ring-green-600 dark:ring-offset-gray-800 dark:focus:ring-offset-gray-800"]]
                label_ [for_ "remember", classes_ ["ms-2 text-sm font-medium text-gray-900 dark:text-gray-300"]] "Remember me"
              a_ [href_ "#", classes_ ["text-sm text-green-700 hover:underline dark:text-green-500"]] "Lost Password?"

            button_ [type_ "submit", classes_ ["w-full text-white bg-green-700 hover:bg-green-800 focus:ring-4 focus:outline-none focus:ring-green-300 font-medium rounded-lg text-sm px-5 py-2.5 text-center dark:bg-green-600 dark:hover:bg-green-700 dark:focus:ring-green-800"]] "Login to your account"
            div_ [classes_ ["text-sm font-medium text-gray-500 dark:text-gray-300"]] $ do
              span_ [class_ "px-2"] "Not registered?"
              a_ [href_ "#", classes_ ["text-green-700 hover:underline dark:text-green-500"]] "Create account"
