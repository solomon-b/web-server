{-# LANGUAGE QuasiQuotes #-}

module API.User.Login.Get where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import Component.Frame (loadFrame)
import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.ByteString (ByteString)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Effects.Observability qualified as Observability
import Log qualified
import OpenTelemetry.Trace qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML, RawHtml, parseFragment, readNodes, renderDocument, renderNodes)
import Text.XmlHtml qualified as Xml
import Text.XmlHtml.Optics

--------------------------------------------------------------------------------

type Route =
  "user"
    :> "login"
    :> Servant.Header "HX-Current-Url" Text
    :> Servant.Header "HX-Request" Bool
    :> Servant.QueryParam "redirect" Text
    :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "Vary" Text] RawHtml)

--------------------------------------------------------------------------------

template :: Maybe Text -> ByteString
template redirectLink =
  [i|<div class="relative p-4 w-full max-w-md max-h-full mx-auto">
  <div class="relative bg-white rounded-lg shadow">
    <div class="flex items-center justify-between p-4 md:p-5 border-b rounded-t">
      <h3 class="text-xl font-semibold text-gray-900">Sign in
      </h3>
    </div>
    <div class="p-4 md:p-5">
      <form hx-post="/user/login#{maybe "" ("?redirect=" <>) redirectLink}" class="space-y-4" data-bitwarden-watching="1">
	<div>
	  <label for="email" class="block mb-2 text-sm font-medium text-gray-900">Your email
	  </label>
	  <input type="email" name="email" id="email" class="bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-green-500 focus:border-green-500 block w-full p-2.5" placeholder="name@company.com">
	</div>
	<div>
	  <label for="password" class="block mb-2 text-sm font-medium text-gray-900">Your password
	  </label>
	  <input type="password" name="password" id="password" placeholder="••••••••" class="bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-green-500 focus:border-green-500 block w-full p-2.5">
	</div>
	<div class="flex justify-between">
	  <div class="flex items-start">
	    <div class="flex items-center h-5">
	      <input id="remember" type="checkbox" value="" class="w-4 h-4 border border-gray-300 rounded bg-gray-50 focus:ring-3 focus:ring-green-300">
	    </div>
	    <label for="remember" class="ms-2 text-sm font-medium text-gray-900">Remember me
	    </label>
	  </div>
	  <a href="\#" class="text-sm text-green-700 hover:underline">Lost Password?
	  </a>
	</div>
	<button type="submit" class="w-full text-white bg-green-700 hover:bg-green-800 focus:ring-4 focus:outline-none focus:ring-green-300 font-medium rounded-lg text-sm px-5 py-2.5 text-center">Login to your account
	</button>
	<div class="text-sm font-medium text-gray-500">
	  <span class="px-2">Not registered?
	  </span>
	  <a href="\#" hx-get="/user/register" hx-swap="innerHTML" hx-target="body" hx-push-url="true" class="text-green-700 hover:underline">Create account</a>
	</div>
      </form>
    </div>
  </div>
</div>
|]

--------------------------------------------------------------------------------

handler ::
  ( Applicative m,
    Has Trace.Tracer env,
    MonadCatch m,
    Log.MonadLog m,
    MonadUnliftIO m,
    MonadReader env m
  ) =>
  Maybe Text ->
  Maybe Bool ->
  Maybe Text ->
  m (Servant.Headers '[Servant.Header "Vary" Text] RawHtml)
handler hxCurrentUrl hxTrigger redirectQueryParam =
  Observability.handlerSpan "GET /user/login" () (display . Servant.getResponse) $ do
    pageFragment <- parseFragment $ template $ hxCurrentUrl <|> redirectQueryParam
    page <- loadFrame pageFragment

    case hxTrigger of
      Just True ->
        pure $ Servant.addHeader "HX-Request" $ renderNodes pageFragment
      _ -> do
        let html = renderDocument $ swapMain pageFragment page
        pure $ Servant.addHeader "HX-Request" html

--------------------------------------------------------------------------------

swapMain :: [Xml.Node] -> Xml.Document -> Xml.Document
swapMain = swapInner _body

readUserAuthFragment :: (MonadIO m, MonadThrow m) => Auth.LoggedIn -> m [Xml.Node]
readUserAuthFragment = \case
  Auth.IsLoggedIn _ -> readNodes "src/Templates/Root/Logout/button.html"
  Auth.IsNotLoggedIn -> liftA2 (<>) (readNodes "src/Templates/Root/Login/button.html") (readNodes "src/Templates/Root/Register/button.html")
