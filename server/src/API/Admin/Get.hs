{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module API.Admin.Get where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import Component.Frame (loadFrameWithNav)
import Control.Lens (set, (<&>))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Text.Encoding qualified as TE
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.MailingList qualified as MailingList
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import Log qualified
import OpenTelemetry.Trace.Core qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML, RawHtml, parseFragment, renderDocument, renderNodes)
import Text.XmlHtml qualified as Xml
import Text.XmlHtml.Optics (_elChildren, _id)

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> Servant.Header "HX-Request" Bool
    :> "admin"
    :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "Vary" Text] RawHtml)

--------------------------------------------------------------------------------

template :: ByteString
template =
  [i|<div class="flex flex-col justify-center items-center w-full">
  <h1 class="mt-3 text-3xl font-extrabold tracking-tight text-slate-900">Admin Page</h1>
  <div id="db-tables" class="p-4 my-8 border border-solid border-gray-200 rounded-lg shadow-md"></div>
</div>
|]

--------------------------------------------------------------------------------
-- Components

userRow :: User.Model -> Text
userRow User.Model {..} =
  [i|<tr class="bg-white border-b">
       <th scope="row" class="px-6 py-4 font-medium text-gray-900 whitespace-nowrap">#{display mId}</th>
       <td class="px-6 py-4">#{display mEmail}</td>
       <td class="px-6 py-4">#{display mDisplayName}</td>
       <td class="px-6 py-4">#{display mAvatarUrl}</td>
       <td class="px-6 py-4">#{display mIsAdmin}</td>
     </tr>
    |]

userTable :: [User.Model] -> Text
userTable entries =
  [i|<div class="relative overflow-x-auto">
       <table class="w-full text-sm text-left rtl:text-right text-gray-500">
         <h3 class="mt-3 text-xl font-extrabold tracking-tight text-slate-900">Users</h3>
         <thead class="text-xs text-gray-700 uppercase bg-gray-50">
           <tr>
             <th scope="col" class="px-6 py-3">id</th>
             <th scope="col" class="px-6 py-3">email</th>
             <th scope="col" class="px-6 py-3">display name</th>
             <th scope="col" class="px-6 py-3">avatar url</th>
             <th scope="col" class="px-6 py-3">is admin</th>
           </tr>
         </thead>
         <tbody>
           #{foldMap userRow entries}
         </tbody>
       </table>
     </div>
    |]

mailingListRow :: MailingList.Model -> Text
mailingListRow MailingList.Model {..} =
  [i|<tr class="bg-white border-b">
       <th scope="row" class="px-6 py-4 font-medium text-gray-900 whitespace-nowrap">#{display mId}</th>
       <td class="px-6 py-4">#{display mEmail}</td>
     </tr>
    |]

mailingListTable :: [MailingList.Model] -> Text
mailingListTable entries =
  [i|<div class="relative overflow-x-auto">
       <table class="w-full text-sm text-left rtl:text-right text-gray-500">
         <h3 class="mt-3 text-xl font-extrabold tracking-tight text-slate-900">Mailing List</h3>
         <thead class="text-xs text-gray-700 uppercase bg-gray-50">
           <tr>
             <th scope="col" class="px-6 py-3">id</th>
             <th scope="col" class="px-6 py-3">email</th>
           </tr>
         </thead>
         <tbody>
           #{foldMap mailingListRow entries}
         </tbody>
       </table>
     </div>
    |]

unauthorized :: BS.ByteString
unauthorized =
  [i|<h1 class="mt-1 text-xl font-extrabold tracking-tight text-slate-900">Unauthorized</h1>|]

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
  m (Servant.Headers '[Servant.Header "Vary" Text] RawHtml)
handler (Auth.Authz user@User.Domain {..} _) hxTrigger = do
  Observability.handlerSpan "GET /admin" () (display . Servant.getResponse) $
    if dIsAdmin
      then do
        users <- execQuerySpanThrow User.getUsers
        let x = TE.encodeUtf8 $ userTable users
        userTableFragment <- parseFragment x

        mailingList <- execQuerySpanThrow MailingList.getEmailListEntries
        mailingListTableFragment <- parseFragment $ TE.encodeUtf8 $ mailingListTable mailingList

        case hxTrigger of
          Just True -> do
            pageFragment <- parseFragment template <&> swapTableFragment (userTableFragment <> mailingListTableFragment)
            let html = renderNodes pageFragment
            pure $ Servant.addHeader "HX-Request" html
          _ -> do
            pageFragment <- parseFragment template <&> swapTableFragment (userTableFragment <> mailingListTableFragment)
            page <- loadFrameWithNav (Auth.IsLoggedIn user) "about-tab" pageFragment
            let html = renderDocument page
            pure $ Servant.addHeader "HX-Request" html
      else renderUnauthorized $ Auth.IsLoggedIn user

swapTableFragment :: [Xml.Node] -> [Xml.Node] -> [Xml.Node]
swapTableFragment x = fmap (set (_id "db-tables" . _elChildren) x)

renderUnauthorized :: (MonadIO m, MonadThrow m) => Auth.LoggedIn -> m (Servant.Headers '[Servant.Header "Vary" Text] RawHtml)
renderUnauthorized loginState = do
  pageFragment <- parseFragment unauthorized
  page <- loadFrameWithNav loginState "about-tab" pageFragment
  let html = renderDocument page
  pure $ Servant.addHeader "HX-Request" html
