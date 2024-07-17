module API.Admin where

--------------------------------------------------------------------------------

import Auth qualified
import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Identity (Identity (..))
import Data.String (IsString (..))
import Data.Text qualified as Text
import Data.Text.Display (display)
import Domain.Types.MailingListEntry (MailingListEntry (..))
import Domain.Types.User
import Effects.Database.Class (MonadDB)
import Effects.Database.Queries.MailingList (selectMailingListEntries)
import Effects.Database.Queries.User
import Effects.Database.Tables.User qualified as User
import Effects.Database.Utils
import Effects.FormBuilder qualified as FB
import Errors (throw401')
import Log qualified
import Lucid qualified
import Lucid.Htmx qualified
import Servant qualified
import Servant.HTML.Lucid qualified as Lucid

--------------------------------------------------------------------------------
-- Route

type ProtectedAPI = Servant.Get '[Lucid.HTML] AdminPage

--------------------------------------------------------------------------------

data AdminPage = AdminPage [User.Model Identity] [MailingListEntry]

instance Lucid.ToHtml AdminPage where
  toHtml :: (Monad m) => AdminPage -> Lucid.HtmlT m ()
  toHtml (AdminPage users mailingListEntries) =
    Lucid.doctypehtml_ $ do
      Lucid.head_ $ do
        Lucid.title_ "Admin"
        Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.type_ "text/css", Lucid.href_ "https://matcha.mizu.sh/matcha.css"]
        Lucid.Htmx.useHtmxVersion (2, 0, 0)
      Lucid.body_ $ do
        Lucid.div_ $ do
          Lucid.h1_ "Admin Page"
          Lucid.section_ $ do
            Lucid.header_ $ Lucid.h3_ "Tables"
            FB.buildForm users
            mailingListTable mailingListEntries

  toHtmlRaw :: (Monad m) => AdminPage -> Lucid.HtmlT m ()
  toHtmlRaw = Lucid.toHtml

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

handler ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m
  ) =>
  Auth.Authz ->
  Servant.ServerT ProtectedAPI m
handler (Auth.Authz User {..} _) = do
  unless userIsAdmin throw401'
  AdminPage <$> execQuerySpanThrowMessage "Failed to query users table" selectUsersQuery <*> selectMailingListEntries
