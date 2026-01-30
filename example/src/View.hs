module View
  ( frame,
    homePage,
    loginPage,
    registerPage,
    dashboardPage,
  )
where

import App.Auth (LoggedIn (..))
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.EmailAddress (EmailAddress)
import Effects.Database.Tables.User qualified as User
import Lucid

--------------------------------------------------------------------------------
-- Base frame

frame :: Html () -> Html ()
frame content =
  doctypehtml_ $ do
    head_ $ do
      title_ "Minimal Example"
      script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Text)
      script_ [src_ "https://unpkg.com/htmx.org@2.0.0"] ("" :: Text)
    body_ [class_ "bg-gray-100 min-h-screen"] $ do
      div_ [class_ "container mx-auto px-4 py-8"] content

--------------------------------------------------------------------------------
-- Home page

homePage :: LoggedIn -> Html ()
homePage loginState = frame $ do
  div_ [class_ "max-w-2xl mx-auto"] $ do
    h1_ [class_ "text-3xl font-bold mb-6 text-center"] "Welcome"
    div_ [class_ "bg-white p-6 rounded-lg shadow"] $ do
      case loginState of
        IsLoggedIn user -> do
          p_ [class_ "text-lg mb-4"] $ do
            "Hello, "
            toHtml (display $ User.mEmail user)
            "!"
          div_ [class_ "flex gap-4"] $ do
            a_ [href_ "/dashboard", class_ "px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600"] "Go to Dashboard"
            form_ [action_ "/user/logout", method_ "post", class_ "inline"] $ do
              button_ [type_ "submit", class_ "px-4 py-2 bg-red-500 text-white rounded hover:bg-red-600"] "Logout"
        IsNotLoggedIn -> do
          p_ [class_ "text-lg mb-4"] "You are not logged in."
          div_ [class_ "flex gap-4"] $ do
            a_ [href_ "/user/login", class_ "px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600"] "Login"
            a_ [href_ "/user/register", class_ "px-4 py-2 bg-green-500 text-white rounded hover:bg-green-600"] "Register"

--------------------------------------------------------------------------------
-- Login page

loginPage :: Maybe EmailAddress -> Maybe Text -> Html ()
loginPage mEmail mRedirect = frame $ do
  div_ [class_ "max-w-md mx-auto"] $ do
    h1_ [class_ "text-3xl font-bold mb-6 text-center"] "Login"
    div_ [class_ "bg-white p-6 rounded-lg shadow"] $ do
      form_
        [ action_ $ "/user/login" <> maybe "" ("?redirect=" <>) mRedirect,
          method_ "post",
          class_ "space-y-4"
        ]
        $ do
          div_ $ do
            label_ [for_ "email", class_ "block text-sm font-medium text-gray-700 mb-1"] "Email"
            input_
              [ type_ "email",
                id_ "email",
                name_ "email",
                value_ $ maybe "" display mEmail,
                required_ "",
                class_ "w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
              ]
          div_ $ do
            label_ [for_ "password", class_ "block text-sm font-medium text-gray-700 mb-1"] "Password"
            input_
              [ type_ "password",
                id_ "password",
                name_ "password",
                required_ "",
                class_ "w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
              ]
          button_
            [ type_ "submit",
              class_ "w-full px-4 py-2 bg-blue-500 text-white rounded-md hover:bg-blue-600"
            ]
            "Login"
      p_ [class_ "mt-4 text-center text-sm text-gray-600"] $ do
        "Don't have an account? "
        a_ [href_ "/user/register", class_ "text-blue-500 hover:underline"] "Register"

--------------------------------------------------------------------------------
-- Register page

registerPage :: Maybe EmailAddress -> Html ()
registerPage mEmail = frame $ do
  div_ [class_ "max-w-md mx-auto"] $ do
    h1_ [class_ "text-3xl font-bold mb-6 text-center"] "Register"
    div_ [class_ "bg-white p-6 rounded-lg shadow"] $ do
      form_ [action_ "/user/register", method_ "post", class_ "space-y-4"] $ do
        div_ $ do
          label_ [for_ "email", class_ "block text-sm font-medium text-gray-700 mb-1"] "Email"
          input_
            [ type_ "email",
              id_ "email",
              name_ "email",
              value_ $ maybe "" display mEmail,
              required_ "",
              class_ "w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
            ]
        div_ $ do
          label_ [for_ "password", class_ "block text-sm font-medium text-gray-700 mb-1"] "Password"
          input_
            [ type_ "password",
              id_ "password",
              name_ "password",
              required_ "",
              class_ "w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
            ]
        button_
          [ type_ "submit",
            class_ "w-full px-4 py-2 bg-green-500 text-white rounded-md hover:bg-green-600"
          ]
          "Register"
      p_ [class_ "mt-4 text-center text-sm text-gray-600"] $ do
        "Already have an account? "
        a_ [href_ "/user/login", class_ "text-blue-500 hover:underline"] "Login"

--------------------------------------------------------------------------------
-- Dashboard page (protected)

dashboardPage :: User.Model -> Html ()
dashboardPage user = frame $ do
  div_ [class_ "max-w-2xl mx-auto"] $ do
    h1_ [class_ "text-3xl font-bold mb-6 text-center"] "Dashboard"
    div_ [class_ "bg-white p-6 rounded-lg shadow"] $ do
      p_ [class_ "text-lg mb-4"] $ do
        "Welcome to your dashboard, "
        toHtml (display $ User.mEmail user)
        "!"
      p_ [class_ "text-gray-600 mb-4"] "This is a protected page. Only logged-in users can see this."
      div_ [class_ "flex gap-4"] $ do
        a_ [href_ "/", class_ "px-4 py-2 bg-gray-500 text-white rounded hover:bg-gray-600"] "Back to Home"
        form_ [action_ "/user/logout", method_ "post", class_ "inline"] $ do
          button_ [type_ "submit", class_ "px-4 py-2 bg-red-500 text-white rounded hover:bg-red-600"] "Logout"
