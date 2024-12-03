{-# LANGUAGE QuasiQuotes #-}

module Component.NavBar where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Errors (InternalServerError (InternalServerError), throwErr)
import Control.Category ((>>>))
import Control.Lens (filtered, preview, set, traversed, (<&>), _Just)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Text.HTML (parseFragment, parseNode)
import Text.XmlHtml qualified as Xml
import Text.XmlHtml.Optics (FocusedElement (..), swapInner', _FocusedElement, _a, _elAttributes, _elChildren', _id')

--------------------------------------------------------------------------------

template :: ByteString
template =
  [i|   <nav id='navbar'>
            <div class='flex flex-wrap justify-between items-center mx-auto p-4'>
                <div class='items-center flex w-auto order-1'>
                    <a href='/' class='flex items-center space-x-3 mr-8'>
                        <span>🌎</span>
                        <span class='text-2xl font-semibold'>HyperNet
        </span>
                    </a>
                    <ul class='flex flex-row space-x-8 font-medium p-0 bg-white'>
                        <li id='home-tab'>
                            <a href='/' class='text-gray-900 hover:text-green-700'>Home
  	</a>
                        </li>
                        <li id='blog-tab'>
                            <a href='/blog' class='text-gray-900 hover:text-green-700'>Blog
  	</a>
                        </li>
                        <li id='about-tab'>
                            <a href='/about' class='text-gray-900 hover:text-green-700'>About
  	</a>
                        </li>
                        <li id='contact-tab'>
                            <a href='\#' class='text-gray-900 hover:text-green-700'>Contact
  	</a>
                        </li>
                    </ul>
                </div>
                <div id='right-nav' class='flex order-2 space-x-3'>
                    <form class='mx-auto'>
                        <label for='default-search' class='mb-2 text-sm font-medium text-gray-900 sr-only'>Search
        </label>
                        <div class='relative'>
                            <div class='absolute inset-y-0 start-0 flex items-center ps-3 pointer-events-none'>
                                <span>🔎</span>
                            </div>
                            <input type='search' id='default-search' class='block w-full p-2 ps-10 text-sm text-gray-900 border border-gray-300 rounded-lg bg-gray-50 focus:ring-green-500 focus:border-green-500' placeholder='Search...' required />
                        </div>
                    </form>
                    <div id='user-auth-links'><button class='py-2 text-gray-900 hover:bg-transparent hover:text-green-700' hx-post='/user/logout' hx-swap='innerHTML' hx-push-url='true'>
  Logout
</button>
                    </div>
                </div>
            </div>
        </nav>
|]

loginButton :: ByteString
loginButton =
  [i|<button class="py-2 text-gray-900 rounded hover:bg-transparent hover:text-green-700" hx-get="/user/login" hx-swap="innerHTML" hx-target="body" hx-push-url="true">
  Login
</button>
|]

logoutButton :: ByteString
logoutButton =
  [i|<button class="py-2 text-gray-900 rounded hover:bg-transparent hover:text-green-700" hx-post="/user/logout" hx-swap="innerHTML" hx-push-url="true">
  Logout
</button>
|]

signupButton :: ByteString
signupButton =
  [i|<button class="text-white bg-green-700 hover:bg-green-800 focus:ring-4 focus:outline-none focus:ring-green-300 font-medium rounded-lg text-sm px-4 py-2 text-center" hx-get="/user/register" hx-swap="innerHTML" hx-target="body" hx-push-url="true">
  Sign Up
</button>
|]

--------------------------------------------------------------------------------

navBar :: (MonadThrow m) => m FocusedElement
navBar =
  case preview (_Just . _FocusedElement) (parseNode template) of
    Nothing -> throwErr InternalServerError
    Just node -> pure node

-- | Update the navbar highlighting.
updateTabHighlight :: Text -> FocusedElement -> FocusedElement
updateTabHighlight tabId =
  set (_id' tabId . _elChildren' . _a . _elAttributes . traversed . filtered (\(k, _) -> k == "class")) ("class", focused)
  where
    focused = "block py-2 px-3 text-white bg-green-700 rounded md:bg-transparent md:text-green-700 md:p-0"

-- | Replace the User Auth Buttons in the Navbar.
updateAuthLinks :: [Xml.Node] -> FocusedElement -> FocusedElement
updateAuthLinks = swapInner' (_id' "user-auth-links")

readUserAuthFragment :: (MonadIO m, MonadThrow m) => Auth.LoggedIn -> m [Xml.Node]
readUserAuthFragment = \case
  Auth.IsLoggedIn -> parseFragment logoutButton
  Auth.IsNotLoggedIn -> liftA2 (<>) (parseFragment loginButton) (parseFragment signupButton)

loadNavBar :: (MonadIO m, MonadThrow m) => Auth.LoggedIn -> Text -> m FocusedElement
loadNavBar loginState tabId = do
  authFragment <- readUserAuthFragment loginState
  navBar <&> (updateTabHighlight tabId >>> updateAuthLinks authFragment)
