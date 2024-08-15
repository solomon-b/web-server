module Widgets.Body where

--------------------------------------------------------------------------------

import Lucid
import Utils.HTML (classes_)
import Widgets.Header qualified as Header
import Widgets.Navbar qualified as Navbar

--------------------------------------------------------------------------------

widget :: (Monad m) => Navbar.LoggedIn -> HtmlT m a -> HtmlT m a
widget loggedIn b =
  doctypehtml_ $ do
    Header.widget
    body_ $ do
      div_ [classes_ ["container", "mx-auto"]] $ do
        Navbar.navBar loggedIn
        main_ [id_ "main", classes_ ["mx-4", "flex", "flex-wrap", "items-center"]] b
