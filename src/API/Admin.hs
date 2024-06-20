module API.Admin where

--------------------------------------------------------------------------------

import API.User (usersHandler)
import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow)
import Data.String (IsString (..))
import Data.Text qualified as Text
import Data.Text.Display (display)
import Domain.Types.MailingListEntry (MailingListEntry (..))
import Domain.Types.User
import Effects.Database.Class (MonadDB)
import Effects.Database.Queries.MailingList (selectMailingListEntries)
import Errors (throw401')
import Log qualified
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

--------------------------------------------------------------------------------

data AdminPage = AdminPage [User] [MailingListEntry]

instance Lucid.ToHtml AdminPage where
  toHtml :: (Monad m) => AdminPage -> Lucid.HtmlT m ()
  toHtml (AdminPage users mailingListEntries) =
    Lucid.doctypehtml_ $ do
      Lucid.head_ $ do
        Lucid.title_ "Admin"
        Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.type_ "text/css", Lucid.href_ "https://matcha.mizu.sh/matcha.css"]
      Lucid.body_ $ do
        Lucid.div_ $ do
          Lucid.h1_ "Admin Page"
          Lucid.section_ $ do
            Lucid.header_ $ Lucid.h3_ "Tables"
            usersTable users
            mailingListTable mailingListEntries

  toHtmlRaw :: (Monad m) => AdminPage -> Lucid.HtmlT m ()
  toHtmlRaw = Lucid.toHtml

userRow :: (Monad m) => User -> Lucid.HtmlT m ()
userRow User {..} =
  Lucid.tr_ $ do
    Lucid.td_ (fromString $ Text.unpack $ display userId)
    Lucid.td_ (fromString $ Text.unpack $ display userEmail)
    Lucid.td_ (fromString $ Text.unpack $ display userDisplayName)
    Lucid.td_ (fromString $ Text.unpack $ display userAvatarUrl)
    Lucid.td_ (fromString $ Text.unpack $ display userIsAdmin)
    Lucid.td_ (Lucid.a_ [Lucid.href_ $ "user/" <> display userId <> "/delete"] "❌")

usersTable :: (Monad m) => [User] -> Lucid.HtmlT m ()
usersTable users = do
  Lucid.div_ [Lucid.class_ "table-responsive"] $ do
    Lucid.table_ [Lucid.class_ "center"] $ do
      Lucid.caption_ "User Table"
      Lucid.thead_ $ do
        Lucid.tr_ $ do
          Lucid.th_ "id"
          Lucid.th_ "email"
          Lucid.th_ "displayName"
          Lucid.th_ "avatarUrl"
          Lucid.th_ "isAdmin"
      Lucid.tbody_ $ do
        foldMap userRow users

mailingListRow :: (Monad m) => MailingListEntry -> Lucid.HtmlT m ()
mailingListRow MailingListEntry {..} =
  Lucid.tr_ $ do
    Lucid.td_ (fromString $ Text.unpack $ display mlId)
    Lucid.td_ (fromString $ Text.unpack $ display mlEmail)

mailingListTable :: (Monad m) => [MailingListEntry] -> Lucid.HtmlT m ()
mailingListTable entries = do
  Lucid.div_ [Lucid.class_ "table-responsive"] $ do
    Lucid.table_ [Lucid.class_ "center"] $ do
      Lucid.caption_ "Mailing List Table"
      Lucid.thead_ $ do
        Lucid.tr_ $ do
          Lucid.th_ "id"
          Lucid.th_ "email"
      Lucid.tbody_ $ do
        foldMap mailingListRow entries

--------------------------------------------------------------------------------
-- Handler

adminPageHandler ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m
  ) =>
  Servant.ServerT AdminAPI m
adminPageHandler (SAS.Authenticated User {..}) = do
  unless userIsAdmin throw401'
  AdminPage <$> usersHandler <*> selectMailingListEntries
adminPageHandler _ = throw401'
