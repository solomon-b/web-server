{-# LANGUAGE QuasiQuotes #-}

module API.Admin.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (adminBlogGetLink)
import App.Auth qualified as Auth
import Component.Frame (adminColumn, loadFrameWithNav, loadFrameWithNavAdmin)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Data.Text.Display (display)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.MailingList qualified as MailingList
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import Log qualified
import Lucid (class_, div_, h1_, h3_, id_, scope_, table_, tbody_, td_, th_, thead_, tr_)
import Lucid qualified
import OpenTelemetry.Trace.Core qualified as Trace
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Link
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> Servant.Header "HX-Request" Bool
    :> "admin"
    :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "Vary" Text] (Lucid.Html ()))

--------------------------------------------------------------------------------

adminBlogGetUrl :: Link.URI
adminBlogGetUrl = Link.linkURI adminBlogGetLink

template :: Lucid.Html () -> Lucid.Html () -> Lucid.Html ()
template userTable' mailingListTable' =
  div_ [class_ "flex-auto", id_ "admin-main"] $ do
    div_ [id_ "db-tables", class_ "p-4 my-8 border border-solid border-gray-200 rounded-lg shadow-md"] $ do
      userTable'
      mailingListTable'

--------------------------------------------------------------------------------

userRow :: User.Model -> Lucid.Html ()
userRow User.Model {..} =
  tr_ [class_ "bg-white border-b"] $ do
    th_ [scope_ "row", class_ "px-6 py-4 font-medium text-gray-900 whitespace-nowrap"] (Lucid.toHtml $ display mId)
    td_ [class_ "px-6 py-4"] (Lucid.toHtml $ display mEmail)
    td_ [class_ "px-6 py-4"] (Lucid.toHtml $ display mDisplayName)
    td_ [class_ "px-6 py-4"] (Lucid.toHtml $ display mAvatarUrl)
    td_ [class_ "px-6 py-4"] (Lucid.toHtml $ display mIsAdmin)

userTable :: [User.Model] -> Lucid.Html ()
userTable entries =
  div_ [class_ "relative overflow-x-auto"] $ do
    table_ [class_ "w-full text-sm text-left rtl:text-right text-gray-500"] $ do
      h3_ [class_ "mt-3 text-xl font-extrabold tracking-tight text-slate-900"] "Users"
      thead_ [class_ "text-xs text-gray-700 uppercase bg-gray-50"] $ do
        tr_ $ do
          th_ [scope_ "col", class_ "px-6 py-3"] "id"
          th_ [scope_ "col", class_ "px-6 py-3"] "email"
          th_ [scope_ "col", class_ "px-6 py-3"] "display name"
          th_ [scope_ "col", class_ "px-6 py-3"] "avatar url"
          th_ [scope_ "col", class_ "px-6 py-3"] "is admin"
      tbody_
        (foldMap userRow entries)

mailingListRow :: MailingList.Model -> Lucid.Html ()
mailingListRow MailingList.Model {..} =
  tr_ [class_ "bg-white border-b"] $ do
    th_ [scope_ "row", class_ "px-6 py-4 font-medium text-gray-900 whitespace-nowrap"] (Lucid.toHtml $ display mId)
    td_ [class_ "px-6 py-4"] (Lucid.toHtml $ display mEmail)

mailingListTable :: [MailingList.Model] -> Lucid.Html ()
mailingListTable entries =
  div_ [class_ "relative overflow-x-auto"] $
    table_ [class_ "w-full text-sm text-left rtl:text-right text-gray-500"] $ do
      h3_ [class_ "mt-3 text-xl font-extrabold tracking-tight text-slate-900"] "Mailing List"
      thead_ [class_ "text-xs text-gray-700 uppercase bg-gray-50"] $
        tr_ $ do
          th_ [scope_ "col", class_ "px-6 py-3"] "id"
          th_ [scope_ "col", class_ "px-6 py-3"] "email"
      tbody_
        (foldMap mailingListRow entries)

unauthorized :: Lucid.Html ()
unauthorized =
  h1_ [class_ "mt-1 text-xl font-extrabold tracking-tight text-slate-900"] "Unauthorized"

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
    MonadUnliftIO m,
    MonadIO m
  ) =>
  Auth.Authz ->
  Maybe Bool ->
  m (Servant.Headers '[Servant.Header "Vary" Text] (Lucid.Html ()))
handler (Auth.Authz user@User.Domain {..} _) hxTrigger = do
  Observability.handlerSpan "GET /admin" () (display . Servant.getResponse) $
    if dIsAdmin
      then do
        users <- execQuerySpanThrow User.getUsers
        let userTableFragment = userTable users

        mailingList <- execQuerySpanThrow MailingList.getEmailListEntries
        let mailingListTableFragment = mailingListTable mailingList

        case hxTrigger of
          Just True -> do
            let page = template userTableFragment mailingListTableFragment
            pure $ Servant.addHeader "HX-Request" (adminColumn page)
          _ -> do
            let page = template userTableFragment mailingListTableFragment
            pageWithFrame <- loadFrameWithNavAdmin (Auth.IsLoggedIn user) "about-tab" page
            pure $ Servant.addHeader "HX-Request" pageWithFrame
      else renderUnauthorized $ Auth.IsLoggedIn user

renderUnauthorized :: (MonadIO m, Log.MonadLog m, MonadThrow m) => Auth.LoggedIn -> m (Servant.Headers '[Servant.Header "Vary" Text] (Lucid.Html ()))
renderUnauthorized loginState = do
  page <- loadFrameWithNav loginState "about-tab" unauthorized
  pure $ Servant.addHeader "HX-Request" page
