{-# LANGUAGE QuasiQuotes #-}

module API.User.Register.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (userRegisterPostLink)
import App.Auth qualified as Auth
import Component.Frame (loadFrame)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import Domain.Types.InvalidField (InvalidField)
import Effects.Observability qualified as Observability
import Log qualified
import OpenTelemetry.Trace qualified as Trace
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Link
import Text.HTML (HTML, RawHtml, parseFragment, readNodes, renderDocument, renderNodes)
import Text.XmlHtml qualified as Xml
import Text.XmlHtml.Optics

--------------------------------------------------------------------------------

type Route =
  "user"
    :> "register"
    :> Servant.Header "HX-Request" Bool
    :> Servant.QueryParam "emailAddress" EmailAddress
    :> Servant.QueryParam "displayName" DisplayName
    :> Servant.QueryParam "fullName" FullName
    :> Servant.QueryParams "invalidField" InvalidField
    :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "Vary" Text] RawHtml)

--------------------------------------------------------------------------------

userRegisterPostUrl :: Link.URI
userRegisterPostUrl = Link.linkURI userRegisterPostLink

displayNameField :: Bool -> Maybe DisplayName -> ByteString
displayNameField isInvalid displayName =
  let inputValue = maybe mempty display displayName
      inputValid :: Text
      inputValid = [i|<input type="displayName" name="displayName" id="displayName" value="#{inputValue}" class="bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-green-500 focus:border-green-500 block w-full p-2.5" placeholder="Tim">|]
      inputInvalid :: Text
      inputInvalid = [i|<input type="displayName" name="displayName" id="displayName" value="#{inputValue}" class="bg-red-50 border border-red-500 text-red-900 placeholder-red-700 text-sm rounded-lg focus:ring-red-500 focus:border-red-500 block w-full p-2.5">|]
   in [i|
<div>
  <label for="displayName" class="block mb-2 text-sm font-medium text-gray-900">What should we call you?
  </label>
  #{bool inputValid inputInvalid isInvalid}
</div>
|]

fullNameField :: Bool -> Maybe FullName -> ByteString
fullNameField isInvalid fullName =
  let inputValue = maybe mempty display fullName
      inputValid :: Text
      inputValid = [i|<input type="displayName" name="fullName" id="fullName" value="#{inputValue}" class="bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-green-500 focus:border-green-500 block w-full p-2.5" placeholder="Tim Berners-Lee">|]
      inputInvalid :: Text
      inputInvalid = [i|<input type="displayName" name="fullName" id="fullName" value="#{inputValue}" class="bg-red-50 border border-red-300 text-red-900 placeholder-red-700 text-sm rounded-lg focus:ring-red-500 focus:border-red-500 block w-full p-2.5" placeholder="Tim Berners-Lee">|]
   in [i|
<div>
  <label for="fullName" class="block mb-2 text-sm font-medium text-gray-900">What is your full name?
  </label>
  #{bool inputValid inputInvalid isInvalid}
</div>
|]

emailField :: Bool -> Maybe EmailAddress -> ByteString
emailField isInvalid emailAddress =
  let inputValue = maybe mempty display emailAddress
      inputValid :: Text
      inputValid = [i|<input type="email" name="email" id="email" value="#{inputValue}" class="bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-green-500 focus:border-green-500 block w-full p-2.5" placeholder="name@company.com">|]
      inputInvalid :: Text
      inputInvalid = [i|<input type="email" name="email" id="email" value="#{inputValue}" class="bg-red-50 border border-red-300 text-red-900 text-placeholder-red-700 text-sm rounded-lg focus:ring-red-500 focus:border-red-500 block w-full p-2.5" placeholder="name@company.com">|]
   in [i|
<div>
  <label for="email" class="block mb-2 text-sm font-medium text-gray-900">Your email
  </label> 
  #{bool inputValid inputInvalid isInvalid}
</div>
|]

passwordField :: Bool -> ByteString
passwordField isInvalid =
  let requirements :: Text
      requirements = bool mempty [i|<p class="mt-2 text-sm text-red-600 dark:text-red-500">password must be between 8-64 characters long</p>|] isInvalid
      inputValid :: Text
      inputValid = [i|<input type="password" name="password" id="password" placeholder="••••••••" class="bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-green-500 focus:border-green-500 block w-full p-2.5">|]
      inputInvalid :: Text
      inputInvalid = [i|<input type="password" name="password" id="password" placeholder="••••••••" class="bg-red-50 border border-red-300 text-red-900 text-placeholder-red-700 text-sm rounded-lg focus:ring-red-500 focus:border-red-500 block w-full p-2.5">|]
   in [i|
<div>
  <label for="password" class="block mb-2 text-sm font-medium text-gray-900">Your password
  </label>
  #{bool inputValid inputInvalid isInvalid}
  #{requirements}
</div>
|]

rememberMeToggle :: ByteString
rememberMeToggle =
  [i|
<div class="flex justify-between">
  <div class="flex items-start">
    <div class="flex items-center h-5">
      <input id="remember" type="checkbox" value="" class="w-4 h-4 border border-gray-300 rounded bg-gray-50 focus:ring-3 focus:ring-green-300">
    </div>
    <label for="remember" class="ms-2 text-sm font-medium text-gray-900">Remember me
    </label>
  </div>
</div>
|]

submitButton :: ByteString
submitButton =
  [i|
<button type="submit" class="w-full text-white bg-green-700 hover:bg-green-800 focus:ring-4 focus:outline-none focus:ring-green-300 font-medium rounded-lg text-sm px-5 py-2.5 text-center">Create your account
</button>
|]

alert :: Text -> ByteString
alert msg =
  [i|
<div class="p-4 mb-4 text-sm text-red-800 rounded-lg bg-red-50 dark:bg-gray-800 dark:text-red-400" role="alert">
  #{msg}
</div>
|]

template :: [InvalidField] -> Maybe DisplayName -> Maybe FullName -> Maybe EmailAddress -> ByteString
template invalidFields displayName fullName emailAddress =
  let validationNotice = if null invalidFields then mempty else alert "Registration failed. Please check your information and try again."
   in [i|<div class="relative p-4 w-full max-w-md max-h-full mx-auto">
  <div class="relative bg-white rounded-lg shadow">
    <div class="flex items-center justify-between p-4 md:p-5 border-b rounded-t">
      <h3 class="text-xl font-semibold text-gray-900">Sign Up
      </h3>
    </div>
    <div class="p-4 md:p-5">
      <form hx-post="/#{userRegisterPostUrl}" class="space-y-4" data-bitwarden-watching="1">
        #{validationNotice}
        #{displayNameField ("DisplayName" `elem` invalidFields) displayName}
        #{fullNameField ("FullName" `elem` invalidFields) fullName}
        #{emailField ("EmailAddress" `elem` invalidFields) emailAddress}
        #{passwordField ("Password" `elem` invalidFields)}
        #{rememberMeToggle}
        #{submitButton}
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
  Maybe Bool ->
  Maybe EmailAddress ->
  Maybe DisplayName ->
  Maybe FullName ->
  [InvalidField] ->
  m (Servant.Headers '[Servant.Header "Vary" Text] RawHtml)
handler hxTrigger emailAddress displayName fullName invalidFields =
  Observability.handlerSpan "GET /user/login" () (display . Servant.getResponse) $ do
    pageFragment <- parseFragment $ template invalidFields displayName fullName emailAddress
    page <- loadFrame pageFragment

    case hxTrigger of
      Just True ->
        pure $ Servant.addHeader "HX-Request" $ renderNodes pageFragment
      _ -> do
        let html = renderDocument $ swapMain pageFragment page
        pure $ Servant.addHeader "HX-Request" html

--------------------------------------------------------------------------------

swapMain :: [Xml.Node] -> Xml.Document -> Xml.Document
swapMain = swapInner _main

readUserAuthFragment :: (MonadIO m, Log.MonadLog m, MonadThrow m) => Auth.LoggedIn -> m [Xml.Node]
readUserAuthFragment = \case
  Auth.IsLoggedIn _ -> readNodes "src/Templates/Root/Logout/button.html"
  Auth.IsNotLoggedIn -> liftA2 (<>) (readNodes "src/Templates/Root/Login/button.html") (readNodes "src/Templates/Root/Register/button.html")
