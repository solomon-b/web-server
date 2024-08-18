module Widgets.Header where

--------------------------------------------------------------------------------

import Htmx.Lucid.Head qualified as Htmx.Lucid
import Lucid

--------------------------------------------------------------------------------

widget :: (Monad m) => HtmlT m ()
widget =
  head_ $ do
    title_ "HyperNet"
    script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Html ())
    Htmx.Lucid.useHtmx
