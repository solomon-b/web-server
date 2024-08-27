{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module API.Admin.Get where

--------------------------------------------------------------------------------

import Auth qualified
import Control.Lens (filtered, set, traversed, (<&>))
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
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
import Errors (Unauthorized (..), throwErr)
import Log qualified
import OpenTelemetry.Trace.Core qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.XmlHtml qualified as Xml
import Text.XmlHtml.Optics (swapInner, _a, _docContent', _elAttributes, _elChildren, _elChildren', _id, _main)
import Utils.HTML (HTML, RawHtml, parseFragment, readDocument, readFragment, renderHTML)

--------------------------------------------------------------------------------

type Route = Servant.AuthProtect "cookie-auth" :> "admin" :> Servant.Get '[HTML] RawHtml

--------------------------------------------------------------------------------

mailingListRow :: MailingList.Model -> Text
mailingListRow MailingList.Model {..} =
  [i|<tr>
       <td>#{display mId}</td>
       <td>#{display mEmail}</td>
     </tr>
    |]

mailingListTable :: [MailingList.Model] -> Text
mailingListTable entries =
  [i|<div>
       <table>
         <caption>Mailing List Table </caption>
         <thead>
         </thead>
           <tr>
             <th>id</id>
             <th>email</id>
           </tr>
         <tbody>
           #{foldMap mailingListRow entries}
         </tbody>
       </table>
     </div>
    |]

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

    authFragment <- readUserAuthFragment Auth.IsLoggedIn

    users <- execQuerySpanThrow User.getUsers

    entries <- execQuerySpanThrow MailingList.getEmailListEntries
    mailingListTableFragment <- parseFragment $ TE.encodeUtf8 $ foldMap mailingListRow entries

    pageFragment <- readFragment "src/Templates/Root/Admin/get.html" <&> swapTableFragment mailingListTableFragment

    template <- readDocument "src/Templates/index.html" <&> updateTabHighlight . updateAuthLinks authFragment . swapMain pageFragment

    pure $ renderHTML template

--------------------------------------------------------------------------------

updateTabHighlight :: Xml.Document -> Xml.Document
updateTabHighlight =
  set (_docContent' . _id "home-tab" . _elChildren' . _a . _elAttributes . traversed . filtered (\(k, _) -> k == "class")) ("class", focused)
  where
    focused = "block py-2 px-3 text-white bg-green-700 rounded md:bg-transparent md:text-green-700 md:p-0"

updateAuthLinks :: [Xml.Node] -> Xml.Document -> Xml.Document
updateAuthLinks = swapInner (_id "user-auth-links")

swapMain :: [Xml.Node] -> Xml.Document -> Xml.Document
swapMain = swapInner _main

swapTableFragment :: [Xml.Node] -> [Xml.Node] -> [Xml.Node]
swapTableFragment x = fmap (set (_id "mailing-list-table" . _elChildren) x)

readUserAuthFragment :: (MonadIO m, MonadThrow m) => Auth.LoggedIn -> m [Xml.Node]
readUserAuthFragment = \case
  Auth.IsLoggedIn -> readFragment "src/Templates/Root/Logout/button.html"
  Auth.IsNotLoggedIn -> liftA2 (<>) (readFragment "src/Templates/Root/Login/button.html") (readFragment "src/Templates/Root/Register/button.html")
