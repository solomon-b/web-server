module API.SplashPage.Get where

--------------------------------------------------------------------------------

import Config (Environment (..))
import Lucid qualified
import Lucid.Htmx qualified
import Servant ((:<|>) (..), (:>))
import Servant qualified
import Servant.HTML.Lucid qualified as Lucid

--------------------------------------------------------------------------------
-- HTML

data SplashPage = SplashPage

instance Lucid.ToHtml SplashPage where
  toHtml :: (Monad m) => SplashPage -> Lucid.HtmlT m ()
  toHtml SplashPage =
    Lucid.doctypehtml_ $ do
      Lucid.head_ $ do
        Lucid.title_ "kpbj.fm"
        Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.type_ "text/css", Lucid.href_ "https://matcha.mizu.sh/matcha.css"]
        Lucid.Htmx.useHtmxVersion (2, 0, 0)
      Lucid.body_ $ do
        Lucid.section_ [Lucid.class_ "centered"] $ do
          Lucid.header_ $ do
            Lucid.a_
              [Lucid.href_ "https://fccdata.org/?call=kpbj&facid=&city=&state=&ccode=1&country=US"]
              (Lucid.img_ [Lucid.src_ "static/range.png"])
            Lucid.p_ "Coming Soon"
          Lucid.div_ $ do
            Lucid.form_ [Lucid.Htmx.hxPost_ "mailing-list/signup", Lucid.Htmx.hxSwap_ "outerHTML"] $ do
              Lucid.input_ [Lucid.type_ "text", Lucid.name_ "emailAddress", Lucid.placeholder_ "email address"]
              Lucid.button_ "submit"

  toHtmlRaw :: (Monad m) => SplashPage -> Lucid.HtmlT m ()
  toHtmlRaw = Lucid.toHtml

type SplashPageAPI =
  Servant.Get '[Lucid.HTML] SplashPage :<|> "static" :> Servant.Raw

--------------------------------------------------------------------------------
-- Handler

handler :: (Monad m) => Environment -> Servant.ServerT SplashPageAPI m
handler = \case
  Production -> pure SplashPage :<|> Servant.serveDirectoryWebApp "/static"
  Development -> pure SplashPage :<|> Servant.serveDirectoryWebApp "./static"
