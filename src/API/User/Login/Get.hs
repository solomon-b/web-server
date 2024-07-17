module API.User.Login.Get where

--------------------------------------------------------------------------------

import Lucid qualified
import Lucid.Htmx qualified
import OrphanInstances ()

--------------------------------------------------------------------------------

data Page = Page

instance Lucid.ToHtml Page where
  toHtml :: (Monad m) => Page -> Lucid.HtmlT m ()
  toHtml Page =
    Lucid.doctypehtml_ $ do
      Lucid.head_ $ do
        Lucid.title_ "Login"
        Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.type_ "text/css", Lucid.href_ "static/styles.css"]
        Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.type_ "text/css", Lucid.href_ "https://matcha.mizu.sh/matcha.css"]
        Lucid.Htmx.useHtmxVersion (2, 0, 0)
      Lucid.body_ $ do
        Lucid.div_ $ do
          Lucid.form_ [Lucid.method_ "POST", Lucid.action_ "login"] $ do
            Lucid.input_ [Lucid.type_ "text", Lucid.name_ "email", Lucid.placeholder_ "email"]
            Lucid.input_ [Lucid.type_ "text", Lucid.name_ "password", Lucid.placeholder_ "password"]
            Lucid.button_ "submit"

  toHtmlRaw :: (Monad m) => Page -> Lucid.HtmlT m ()
  toHtmlRaw = Lucid.toHtml

--------------------------------------------------------------------------------

handler :: (Applicative m) => m (Lucid.Html ())
handler = pure $ Lucid.toHtml Page
