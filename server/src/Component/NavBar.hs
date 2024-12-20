{-# LANGUAGE QuasiQuotes #-}

module Component.NavBar where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (aboutGetLink, adminGetLink, blogGetLink, blogNewGetLink, rootGetLink, staticGetLink, userLoginGetLink, userLogoutPostLink, userRegisterGetLink)
import App.Auth qualified as Auth
import App.Errors (InternalServerError (InternalServerError), throwErr)
import Control.Lens (preview, _Just)
import Control.Monad.Catch (MonadThrow)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Effects.Database.Tables.User qualified as User
import Log qualified
import Servant.Links qualified as Link
import Text.HTML (parseNode)
import Text.XmlHtml.Optics (FocusedElement (..), _FocusedElement)

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
userRegisterGetUrl = Link.linkURI (userRegisterGetLink Nothing Nothing Nothing [])

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

userDropdown :: User.Domain -> ByteString
userDropdown User.Domain {dId, dDisplayName, dAvatarUrl} =
  let avatar = fromMaybe "/#{staticGetUrl}/avatar.png" dAvatarUrl
   in [i|
                <button id='adminUserButton' hx-get='/user/#{display dId}' hx-target='\#main' hx-push-url='true' data-dropdown-toggle='adminUserDropdown' data-dropdown-offset-distance='0' data-dropdown-trigger='hover' class='font-medium text-sm p-2.5 text-center inline-flex items-center' type='button'>
                  <img src='#{avatar}' class='size-4' />
                  <span class='ps-2'>Hello, #{display dDisplayName}</span>
                </button>
                
                <div id='adminUserDropdown' class='z-10 hidden bg-white divide-y divide-gray-100'>
                    <ul class='text-sm text-gray-700' aria-labelledby='adminUserButton'>
                      <li>
                        <button class='block px-4 py-2 hover:bg-gray-100'>Settings</button>
                      </li>
                      <li>
                        <button class='block px-4 py-2 hover:bg-gray-100'>Edit Profile</button>
                      </li>
                    </ul>
                    <div class='py-2 text-sm text-gray-700'>
                        <button hx-post="/#{userLogoutPostUrl}" hx-swap="innerHTML" hx-push-url="true" class='block px-4 py-2 hover:bg-gray-100'>Logout</button>
                    </div>
                </div>
|]

adminNavBar :: Auth.LoggedIn -> ByteString
adminNavBar = \case
  (Auth.IsLoggedIn user@User.Domain {dIsAdmin})
    | dIsAdmin ->
        [i|
<div id='admin-nav' class='w-full flex flex-wrap justify-between items-center bg-red-400'>
  <div>
    <button hx-get='/#{adminGetUrl}' hx-target='\#main' hx-push-url='true' class='font-medium text-sm p-2.5 text-center inline-flex items-center me-1'>
      <i class='fa-solid fa-gauge pe-2'></i>
      Dashboard
    </button>
    <button id='adminNewButton' data-dropdown-toggle='adminNewDropdown' data-dropdown-offset-distance='0' data-dropdown-trigger='hover' class='font-medium text-sm p-2.5 text-center inline-flex items-center' type='button'>
      <i class='fa-solid fa-plus pe-2'></i>
      <span class='ps-2'>New</span>
    </button>
    
    <div id='adminNewDropdown' class='z-10 hidden bg-white divide-y divide-gray-100'>
        <ul class='text-sm text-gray-700' aria-labelledby='adminNewButton'>
          <li>
            <button hx-get='/#{blogNewGetUrl}' hx-target='\#main' hx-push-url='true' class='block px-4 py-2 hover:bg-gray-100'>Blog Post</button>
          </li>
          <li>
            <button class='block px-4 py-2 hover:bg-gray-100'>Event</button>
          </li>
        </ul>
    </div>

  </div>
  <div>
    #{userDropdown user}
  </div>
</div>
|]
  _ -> mempty

loginSignupButton :: ByteString
loginSignupButton =
  [i|<button class="py-2 text-gray-900 rounded hover:bg-transparent hover:text-green-700" hx-get="/#{userLoginGetUrl Nothing}" hx-swap="innerHTML" hx-target="body" hx-push-url="true"> Login</button>
<button class="text-white bg-green-700 hover:bg-green-800 focus:ring-4 focus:outline-none focus:ring-green-300 font-medium rounded-lg text-sm px-4 py-2 text-center" hx-get="/#{userRegisterGetUrl}" hx-swap="innerHTML" hx-target="body" hx-push-url="true">Sign Up</button>
|]

logoutButton :: ByteString
logoutButton =
  [i|<button class="py-2 text-gray-900 rounded hover:bg-transparent hover:text-green-700" hx-post="/#{userLogoutPostUrl}" hx-swap="innerHTML" hx-push-url="true">Logout</button>
|]

tabs :: Text -> ByteString
tabs tabId =
  let focused :: ByteString
      focused = "block py-2 px-3 text-white bg-green-700 rounded md:bg-transparent md:text-green-700 md:p-0"

      unfocused :: ByteString
      unfocused = "text-gray-900 hover:text-green-700"
   in [i|<ul class='flex flex-row space-x-8 font-medium p-0 bg-white'>
       <li id='home-tab'>
           <a href='/#{rootGetUrl}' class='#{bool unfocused focused (tabId == "home-tab")}'>Home</a>
       </li>
       <li id='blog-tab'>
           <a href='/#{blogGetUrl}' class='#{bool unfocused focused (tabId == "blog-tab")}'>Blog</a>
       </li>
       <li id='store-tab'>
           <a href='/#{storeGetUrl}' class='#{bool unfocused focused (tabId == "store-tab")}'>Store</a>
       </li>
       <li id='about-tab'>
           <a href='/#{aboutGetUrl}' class='#{bool unfocused focused (tabId == "about-tab")}'>About</a>
       </li>
       <li id='contact-tab'>
           <a href='/#{contactGetUrl}' class='#{bool unfocused focused (tabId == "contact-tab")}'>Contact</a>
       </li>
</ul>
|]

navbar :: Auth.LoggedIn -> Text -> ByteString
navbar loginState tabId =
  [i|   <nav id='navbar' class='flex flex-col'>
           #{adminNavBar loginState}
            <div class='flex flex-wrap w-full justify-between items-center mx-auto p-4'>
                <div class='items-center flex w-auto order-1'>
                    <a href='/' class='flex items-center space-x-3 mr-8'>
                        <i class="fa-solid fa-globe"></i>
                        <span class='text-2xl font-semibold'>HyperNet
        </span>
                    </a>
                    #{tabs tabId}
                </div>
                <div id='right-nav' class='flex order-2 space-x-3'>
                    <form class='mx-auto'>
                        <label for='default-search' class='mb-2 text-sm font-medium text-gray-900 sr-only'>Search
        </label>
                        <div class='relative'>
                            <div class='absolute inset-y-0 start-0 flex items-center ps-3 pointer-events-none'>
                                <i class="fa-solid fa-magnifying-glass"></i>
                            </div>
                            <input type='search' id='default-search' class='block w-full p-2 ps-10 text-sm text-gray-900 border border-gray-300 rounded-lg bg-gray-50 focus:ring-green-500 focus:border-green-500' placeholder='Search...' required />
                        </div>
                    </form>
                    <div id='user-auth-links'>
                       #{bool loginSignupButton logoutButton (Auth.isLoggedIn loginState)}
                    </div>
                </div>
            </div>
        </nav>
|]

--------------------------------------------------------------------------------

loadNavBar ::
  (Log.MonadLog m, MonadThrow m) =>
  Auth.LoggedIn ->
  Text ->
  m FocusedElement
loadNavBar loginState tabId =
  case preview (_Just . _FocusedElement) (parseNode $ navbar loginState tabId) of
    Nothing -> throwErr $ InternalServerError "Failed to construct Navbar Element from template."
    Just node -> pure node
