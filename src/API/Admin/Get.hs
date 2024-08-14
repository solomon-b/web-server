{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module API.Admin.Get where

--------------------------------------------------------------------------------

import Auth qualified
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display, ShowInstance (..), display)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.MailingList qualified as MailingList
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import Errors (Unauthorized (..), throwErr)
import Htmx.Lucid.Head qualified as Lucid.Htmx
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace.Core qualified as Trace
import Servant ((:>))
import Servant qualified
import Utils.HTML (HTML, RawHtml, toHTML)

--------------------------------------------------------------------------------

type Route = Servant.AuthProtect "cookie-auth" :> "admin" :> Servant.Get '[HTML] RawHtml

--------------------------------------------------------------------------------

data AdminPage = AdminPage [User.Model] [MailingList.Model]
  deriving stock (Show)

deriving via (ShowInstance AdminPage) instance (Display AdminPage)

adminPage :: (Monad m) => AdminPage -> Lucid.HtmlT m ()
adminPage (AdminPage _users mailingListEntries) =
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
          -- FB.buildForm users
          mailingListTable mailingListEntries

mailingListRow :: (Monad m) => MailingList.Model -> Lucid.HtmlT m ()
mailingListRow MailingList.Model {..} =
  Lucid.tr_ $ do
    Lucid.td_ (fromString $ Text.unpack $ display mId)
    Lucid.td_ (fromString $ Text.unpack $ display mEmail)

mailingListTable :: (Monad m) => [MailingList.Model] -> Lucid.HtmlT m ()
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
  forall m env.
  ( Has Trace.Tracer env,
    Log.MonadLog m,
    MonadCatch m,
    MonadDB m,
    MonadReader env m,
    MonadThrow m,
    MonadUnliftIO m
  ) =>
  Auth.Authz ->
  m RawHtml
handler (Auth.Authz User.Domain {..} _) = do
  Observability.handlerSpan "GET /admin" () (const @Text "RawHTML") $ do
    unless dIsAdmin (throwErr Unauthorized)
    x <- AdminPage <$> (execQuerySpanThrow @_ @_ @env) User.getUsers <*> execQuerySpanThrow MailingList.getEmailListEntries
    pure $ toHTML $ adminPage x
