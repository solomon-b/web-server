module API.Admin where

--------------------------------------------------------------------------------

import Config (Environment (..))
import Control.Monad.Catch (MonadThrow)
import Domain.Types.User
import Errors (throw401')
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth (Auth)
import Servant.Auth qualified
import Servant.Auth.Server qualified as SAS
import Servant.HTML.Lucid qualified as Lucid

--------------------------------------------------------------------------------
-- Route

type AdminAPI =
  Auth '[Servant.Auth.JWT, Servant.Auth.Cookie] User :> Servant.Get '[Lucid.HTML] AdminPage

data AdminPage = AdminPage

instance Lucid.ToHtml AdminPage where
  toHtml :: (Monad m) => AdminPage -> Lucid.HtmlT m ()
  toHtml AdminPage =
    Lucid.doctypehtml_ $ do
      Lucid.head_ $ do
        Lucid.title_ "Admin"
        Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.type_ "text/css", Lucid.href_ "static/styles.css"]
        Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.type_ "text/css", Lucid.href_ "https://matcha.mizu.sh/matcha.css"]
      Lucid.body_ $ do
        Lucid.header_ $ do
          Lucid.aside_ "Admin Page Coming Soon"

  toHtmlRaw :: (Monad m) => AdminPage -> Lucid.HtmlT m ()
  toHtmlRaw = Lucid.toHtml

--------------------------------------------------------------------------------
-- Handler

adminPageHandler :: (MonadThrow m) => Environment -> Servant.ServerT AdminAPI m
adminPageHandler _ (SAS.Authenticated User {..}) =
  if userIsAdmin
    then pure AdminPage
    else throw401'
adminPageHandler _ _ = throw401'
