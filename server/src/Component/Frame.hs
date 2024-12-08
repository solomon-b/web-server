{-# LANGUAGE QuasiQuotes #-}

module Component.Frame where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import Component.NavBar (loadNavBar)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.String.Interpolate (i)
import Data.Text (Text)
import Text.HTML (parseDocument')
import Text.XmlHtml qualified as Xml
import Text.XmlHtml.Optics

--------------------------------------------------------------------------------

template :: ByteString
template =
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
      <nav id="navbar"></nav>
      <main id="main" class="mx-4 flex flex-wrap items-center">
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

--------------------------------------------------------------------------------

loadFrame :: (MonadThrow m) => [Xml.Node] -> m Xml.Document
loadFrame tab = parseDocument' template <&> swapInner _main tab

loadFrameWithNav :: (MonadIO m, MonadThrow m) => Auth.LoggedIn -> Text -> [Xml.Node] -> m Xml.Document
loadFrameWithNav loginState tabId tab = do
  frame' <- parseDocument' template <&> swapInner _main tab
  navBar' <- loadNavBar loginState tabId
  pure $ swapOuter _nav navBar' frame'
