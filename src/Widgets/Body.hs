module Widgets.Body where

--------------------------------------------------------------------------------

import Auth (LoggedIn)
import Lucid
import Utils.HTML (classes_)
import Widgets.Header qualified as Header
import Widgets.Navbar qualified as Navbar

--------------------------------------------------------------------------------

widget :: (Monad m) => LoggedIn -> HtmlT m () -> HtmlT m ()
widget loggedIn b =
  doctypehtml_ $ do
    Header.widget
    body_ $ do
      div_ [id_ "shell", classes_ ["container", "mx-auto"]] $ do
        Navbar.navBar loggedIn
        main_ [id_ "main", classes_ ["mx-4", "flex", "flex-wrap", "items-center"]] b
        footer_ [classes_ ["bg-white", "dark:bg-gray-900", "m-4"]] $
          div_ [classes_ ["w-full", "max-w-screen-xl", "mx-auto", "p-4", "md:py-8"]] $ do
            hr_ [classes_ ["my-6", "border-gray-200", "sm:mx-auto", "dark:border-gray-700", "lg:my-8"]]
            span_ [classes_ ["block", "text-sm", "text-gray-500", "sm:text-center", "dark:text-gray-400"]] $ do
              "© 1992 HyperNet™. No Rights Reserved."
