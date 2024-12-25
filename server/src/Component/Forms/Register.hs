{-# LANGUAGE QuasiQuotes #-}

module Component.Forms.Register where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (userLoginPostLink, userRegisterGetLink)
import Data.ByteString
import Data.String.Interpolate (i)
import Data.Text
import Data.Text.Display (display)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import Servant.Links qualified as Link

--------------------------------------------------------------------------------

userLoginPostUrl :: Maybe Text -> Link.URI
userLoginPostUrl = Link.linkURI . userLoginPostLink

userRegisterGetUrl :: Link.URI
userRegisterGetUrl = Link.linkURI (userRegisterGetLink Nothing Nothing Nothing)

displayNameField :: ByteString
displayNameField =
  [i|
<div>
  <label for="displayName" class="block mb-2 text-sm font-medium text-gray-900">What should we call you?
  </label>
  <input
    type="displayName"
    name="displayName"
    id="displayName"
    class="border text-sm rounded-lg block w-full p-2.5"
    x-model="fields.displayName.value"
    x-data="alpineHandler"
    :class="fields.displayName.isValid ? 'bg-gray-50 border-gray-300 focus:ring-green-500 focus:border-green-500' : 'bg-red-50 border-red-900 focus:ring-red-500 focus:border-red-500'"
    @blur="validateField('displayName')"
    @input="validateField('displayName')"
  >
</div>
|]

fullNameField :: ByteString
fullNameField =
  [i|
<div>
  <label for="fullName" class="block mb-2 text-sm font-medium text-gray-900">What is your full name?
  </label>
  <input
    type="fullName"
    name="fullName"
    id="fullName"
    class="border text-sm rounded-lg block w-full p-2.5"
    x-model="fields.fullName.value"
    x-data="alpineHandler"
    :class="fields.fullName.isValid ? 'bg-gray-50 border-gray-300 focus:ring-green-500 focus:border-green-500' : 'bg-red-50 border-red-900 focus:ring-red-500 focus:border-red-500'"
    @blur="validateField('fullName')"
    @input="validateField('fullName')"
    >
</div>
|]

emailField :: ByteString
emailField =
  [i|
<div>
  <label for="email" class="block mb-2 text-sm font-medium text-gray-900">Your email
  </label>
  <input
    type="email"
    name="email"
    id="email"
    class="border text-sm rounded-lg block w-full p-2.5"
    placeholder="name@company.com"
    x-model="fields.email.value"
    x-data="alpineHandler"
    :class="fields.email.isValid ? 'bg-gray-50 border-gray-300 focus:ring-green-500 focus:border-green-500' : 'bg-red-50 border-red-900 focus:ring-red-500 focus:border-red-500'"
    @blur="validateField('email')"
    @input="validateField('email')"
  >
</div>
|]

passwordField :: ByteString
passwordField =
  [i|
<div>
  <label for="password" class="block mb-2 text-sm font-medium text-gray-900">Your password
  </label>
  <input
    type="password"
    name="password"
    id="password" placeholder="••••••••"
    class="bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-green-500 focus:border-green-500 block w-full p-2.5"
    x-model="fields.password.value"
    x-data="alpineHandler"
    :class="fields.password.isValid ? 'bg-gray-50 border-gray-300 focus:ring-green-500 focus:border-green-500' : 'bg-red-50 border-red-900 focus:ring-red-500 focus:border-red-500'"
    @blur="validateField('password')"
    @input="validateField('password')"
  >
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
<button
  type="submit"
  class="w-full text-white bg-green-700 hover:bg-green-800 focus:ring-4 focus:outline-none focus:ring-green-300 font-medium rounded-lg text-sm px-5 py-2.5 text-center"
  x-data="alpineHandler"
  :disabled="!allValid()"
  disabled
>
  Create your account
</button>
|]

alert :: Text -> ByteString
alert msg =
  [i|
<div class="p-4 mb-4 text-sm text-red-800 rounded-lg bg-red-50 dark:bg-gray-800 dark:text-red-400" role="alert">
  #{msg}
</div>
|]

template :: Maybe DisplayName -> Maybe FullName -> Maybe EmailAddress -> Maybe Text -> ByteString
template displayName fullName emailAddress _redirectLink =
  let validationNotice = maybe "" (const $ alert "Your email address or password is invalid.") emailAddress
      displayNameValue = maybe "" display displayName
      fullNameValue = maybe "" display fullName
      emailValue = maybe "" display emailAddress
   in [i|
<div
  class="relative p-4 w-full max-w-md max-h-full mx-auto"
  x-data="{ fields: { displayName: { value: `#{displayNameValue}`, isValid: true }, fullName: { value: `#{fullNameValue}`, isValid: true }, email: { value: `#{emailValue}`, isValid: true }, password: { value: ``, isValid: true }}}"
>
  <div class="relative bg-white rounded-lg shadow">
    <div class="flex items-center justify-between p-4 md:p-5 border-b rounded-t">
      <h3 class="text-xl font-semibold text-gray-900">Sign in
      </h3>
    </div>
    <div class="p-4 md:p-5">
      <form hx-post="/#{userRegisterGetUrl}" class="space-y-4" data-bitwarden-watching="1">
        #{validationNotice}
        #{displayNameField}
        #{fullNameField}
        #{emailField}
        #{passwordField}
        #{rememberMeToggle}
        #{submitButton}
      </form>
      #{javascript}
    </div>
  </div>
</div>
|]

javascript :: ByteString
javascript =
  [i|
<script>
  function validateField(field) {
     this.fields[field].isValid = this.fields[field].value.trim() !== '';
  };

  function allValid() {
    return Object.values(this.fields).every(field => field.isValid && field.value.trim() !== '');
  };

  /------------------------------------------------------------------------------/

  function alpineHandler() {
    return {
      validateField,
      allValid,
    };
  }
</script>
|]
