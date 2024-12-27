{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}

module Component.NavBar where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (aboutGetLink, adminGetLink, blogGetLink, blogNewGetLink, rootGetLink, staticGetLink, userLoginGetLink, userLogoutPostLink, userRegisterGetLink)
import App.Auth qualified as Auth
import Control.Monad.Catch (MonadThrow)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Effects.Database.Tables.User qualified as User
import Log qualified
import Lucid (a_, button_, class_, div_, for_, form_, href_, i_, id_, img_, input_, label_, li_, nav_, placeholder_, required_, span_, src_, type_, ul_)
import Lucid qualified
import Lucid.Extras
import Servant.Links qualified as Link

--------------------------------------------------------------------------------
-- Components

rootGetUrl :: Link.URI
rootGetUrl = Link.linkURI rootGetLink

staticGetUrl :: Link.URI
staticGetUrl = Link.linkURI staticGetLink

adminGetUrl :: Link.URI
adminGetUrl = Link.linkURI adminGetLink

blogNewGetUrl :: Link.URI
blogNewGetUrl = Link.linkURI blogNewGetLink

userLoginGetUrl :: Maybe Text -> Link.URI
userLoginGetUrl = Link.linkURI . flip userLoginGetLink Nothing

userRegisterGetUrl :: Link.URI
userRegisterGetUrl = Link.linkURI (userRegisterGetLink Nothing Nothing Nothing)

userLogoutPostUrl :: Link.URI
userLogoutPostUrl = Link.linkURI userLogoutPostLink

blogGetUrl :: Link.URI
blogGetUrl = Link.linkURI blogGetLink

storeGetUrl :: Link.URI
storeGetUrl = Link.linkURI rootGetLink

aboutGetUrl :: Link.URI
aboutGetUrl = Link.linkURI aboutGetLink

contactGetUrl :: Link.URI
contactGetUrl = Link.linkURI rootGetLink

userDropdown :: User.Domain -> Lucid.Html ()
userDropdown User.Domain {dId, dDisplayName, dAvatarUrl} =
  let avatar = fromMaybe "/#{staticGetUrl}/avatar.png" dAvatarUrl
   in div_ do
        button_ [id_ "adminUserButton", hxGet_ [i|/user/#{display dId}|], hxTarget_ "#main", hxPushUrl_ "true", dataDropdownToggle_ "adminUserDropdown", dataDropdownOffsetDistance_ "0", dataDropdownTrigger_ "hover", class_ "font-medium text-sm p-2.5 text-center inline-flex items-center", type_ "button"] do
          img_ [src_ avatar, class_ "size-4"]
          span_ [class_ "ps-2"] do
            [i|Hello, #{display dDisplayName}|]

        div_ [id_ "adminUserDropdown", class_ "z-10 hidden bg-white divide-y divide-gray-100"] do
          ul_ [class_ "text-sm text-gray-700", ariaLabelledby_ "adminUserButton"] do
            li_ $
              button_ [class_ "block px-4 py-2 hover:bg-gray-100"] "Settings"
            li_ $
              button_ [class_ "block px-4 py-2 hover:bg-gray-100"] "Edit Profile"
          div_ [class_ "py-2 text-sm text-gray-700"] $
            button_ [hxPost_ [i|/#{userLogoutPostUrl}|], hxSwap_ "innerHTML", hxPushUrl_ "true", class_ "block px-4 py-2 hover:bg-gray-100"] "Logout"

adminNavBar :: Auth.LoggedIn -> Lucid.Html ()
adminNavBar = \case
  (Auth.IsLoggedIn user@User.Domain {dIsAdmin})
    | dIsAdmin ->
        div_ [id_ "admin-nav", class_ "w-full flex flex-wrap justify-between items-center bg-red-400"] do
          div_ do
            button_ [hxGet_ [i|/#{adminGetUrl}|], hxTarget_ "#main", hxPushUrl_ "true", class_ "font-medium text-sm p-2.5 text-center inline-flex items-center me-1"] do
              i_ [class_ "fa-solid fa-gauge pe-2"] mempty
              "Dashboard"

            button_ [id_ "adminNewButton", dataDropdownToggle_ "adminNewDropdown", dataDropdownOffsetDistance_ "0", dataDropdownTrigger_ "hover", class_ "font-medium text-sm p-2.5 text-center inline-flex items-center", type_ "button"] do
              i_ [class_ "fa-solid fa-plus pe-2"] mempty
              span_ [class_ "ps-2"] "New"

            div_ [id_ "adminNewDropdown", class_ "z-10 hidden bg-white divide-y divide-gray-100"] $
              ul_ [class_ "text-sm text-gray-700", ariaLabelledby_ "adminNewButton"] do
                li_ $
                  button_ [hxGet_ [i|/#{blogNewGetUrl}|], hxTarget_ "#main", hxPushUrl_ "true", class_ "block px-4 py-2 hover:bg-gray-100"] "Blog Post"
                li_ $
                  button_ [class_ "block px-4 py-2 hover:bg-gray-100"] "Event"

          div_ $
            userDropdown user
  _ -> mempty

loginSignupButton :: Lucid.Html ()
loginSignupButton = do
  button_
    [ class_ "py-2 text-gray-900 rounded hover:bg-transparent hover:text-green-700",
      hxGet_ [i|/#{userLoginGetUrl Nothing}|],
      hxSwap_ "innerHTML",
      hxTarget_ "body",
      hxPushUrl_ "true"
    ]
    "Login"
  button_
    [ class_ "text-white bg-green-700 hover:bg-green-800 focus:ring-4 focus:outline-none focus:ring-green-300 font-medium rounded-lg text-sm px-4 py-2 text-center",
      hxGet_ [i|/#{userRegisterGetUrl}|],
      hxSwap_ "innerHTML",
      hxTarget_ "body",
      hxPushUrl_ "true"
    ]
    "Sign Up"

logoutButton :: Lucid.Html ()
logoutButton =
  button_ [class_ "py-2 text-gray-900 rounded hover:bg-transparent hover:text-green-700", hxPost_ [i|/#{userLogoutPostUrl}|], hxSwap_ "innerHTML", hxPushUrl_ "true"] "Logout"

tabs :: Text -> Lucid.Html ()
tabs tabId =
  let focused :: Text
      focused = "block py-2 px-3 text-white bg-green-700 rounded md:bg-transparent md:text-green-700 md:p-0"

      unfocused :: Text
      unfocused = "text-gray-900 hover:text-green-700"
   in ul_ [class_ "flex flex-row space-x-8 font-medium p-0 bg-white"] do
        li_ [id_ "home-tab"] $
          a_ [href_ [i|/#{rootGetUrl}|], class_ (bool unfocused focused (tabId == "home-tab"))] "Home"
        li_ [id_ "blog-tab"] $
          a_ [href_ [i|/#{blogGetUrl}|], class_ (bool unfocused focused (tabId == "blog-tab"))] "Blog"
        li_ [id_ "store-tab"] $
          a_ [href_ [i|/#{storeGetUrl}|], class_ (bool unfocused focused (tabId == "store-tab"))] "Store"
        li_ [id_ "about-tab"] $
          a_ [href_ [i|/#{aboutGetUrl}|], class_ (bool unfocused focused (tabId == "about-tab"))] "About"
        li_ [id_ "contact-tab"] $
          a_ [href_ [i|/#{contactGetUrl}|], class_ (bool unfocused focused (tabId == "contact-tab"))] "Contact"

navbar :: Auth.LoggedIn -> Text -> Lucid.Html ()
navbar loginState tabId =
  nav_ [id_ "navbar", class_ "flex flex-col"] do
    adminNavBar loginState
    div_ [class_ "flex flex-wrap w-full justify-between items-center mx-auto p-4"] do
      div_ [class_ "items-center flex w-auto order-1"] do
        a_ [href_ "/", class_ "flex items-center space-x-3 mr-8"] do
          i_ [class_ "fa-solid fa-globe"] mempty
          span_ [class_ "text-2xl font-semibold"] "HyperNet"
        tabs tabId

      div_ [id_ "right-nav", class_ "flex order-2 space-x-3"] do
        form_ [class_ "mx-auto"] do
          label_ [for_ "default-search", class_ "mb-2 text-sm font-medium text-gray-900 sr-only"] "Search"
          div_ [class_ "relative"] do
            div_ [class_ "absolute inset-y-0 start-0 flex items-center ps-3 pointer-events-none"] $
              i_ [class_ "fa-solid fa-magnifying-glass"] mempty
            input_ [type_ "search", id_ "default-search", class_ "block w-full p-2 ps-10 text-sm text-gray-900 border border-gray-300 rounded-lg bg-gray-50 focus:ring-green-500 focus:border-green-500", placeholder_ "Search...", required_ "true"]
        div_ [id_ "user-auth-links"] $
          bool loginSignupButton logoutButton (Auth.isLoggedIn loginState)

--------------------------------------------------------------------------------

loadNavBar ::
  (Log.MonadLog m, MonadThrow m) =>
  Auth.LoggedIn ->
  Text ->
  m (Lucid.Html ())
loadNavBar loginState tabId =
  pure $ navbar loginState tabId
