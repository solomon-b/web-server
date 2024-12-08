{-# LANGUAGE QuasiQuotes #-}

module App.Errors.HTML where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import Component.NavBar (navbar)
import Data.ByteString.Lazy qualified as BL
import Data.String.Interpolate (i)

--------------------------------------------------------------------------------

error401template :: BL.ByteString
error401template =
  template
    [i|
<div class='flex flex-col mx-auto mt-8'>
  <h1 class='text-5xl font-extrabold dark:text-white'>Unauthorized <small class="ms-2 font-semibold text-gray-500 dark:text-gray-400">401</small></h1>
  <p>The requested resource requires an authentication.</p>
</div>
|]

error403template :: BL.ByteString
error403template =
  template
    [i|
<div class='flex flex-col mx-auto mt-8'>
  <h1 class='text-5xl font-extrabold dark:text-white'>Access Denied <small class="ms-2 font-semibold text-gray-500 dark:text-gray-400">403</small></h1>
  <p>The requested resource requires an authentication.</p>
</div>
|]

error404template :: BL.ByteString
error404template =
  template
    [i|
<div class='flex flex-col mx-auto mt-8'>
  <h1 class='text-5xl font-extrabold dark:text-white'>Resource not found <small class="ms-2 font-semibold text-gray-500 dark:text-gray-400">404</small></h1>
  <p>The requested resource could not be found but may be available again in the future.</p>
</div>
|]

error500template :: BL.ByteString
error500template =
  template
    [i|
<div class='flex flex-col mx-auto mt-8'>
  <h1 class='text-5xl font-extrabold dark:text-white'>Webservice currently unavailable <small class="ms-2 font-semibold text-gray-500 dark:text-gray-400">500</small></h1>
  <p>An unexpected condition was encountered.</p>
</div>
|]

--------------------------------------------------------------------------------

template :: BL.ByteString -> BL.ByteString
template body =
  [i|<!DOCTYPE HTML>
<html lang="en">
  <head>
    <title>HyperNet
    </title>
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.7.1/css/all.min.css">
    <script src="https://cdn.tailwindcss.com"></script>
    <script src="https://unpkg.com/htmx.org@2.0.0"></script>
    <script src="https://cdn.jsdelivr.net/npm/flowbite@2.5.2/dist/flowbite.min.js"></script>
  </head>
  <body>
    <div id="shell" class="container mx-auto">
      #{navbar Auth.IsNotLoggedIn ""}
      <main id="main" class="mx-4 flex flex-wrap items-center">
        #{body}
      </main>
      <footer class="bg-white dark:bg-gray-900 m-4">
	<div class="w-full max-w-screen-xl mx-auto p-4 md:py-8">
	  <hr class="my-6 border-gray-200 sm:mx-auto dark:border-gray-700 lg:my-8">
	  <span class="block text-sm text-gray-500 sm:text-center dark:text-gray-400">© 1992 HyperNet™. No Rights Reserved.
	  </span>
	</div>
      </footer>
    </div>
  </body>
</html>
|]
