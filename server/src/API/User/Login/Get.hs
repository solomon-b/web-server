module API.User.Login.Get where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import Component.Forms.Login (template)
import Component.Frame (loadFrame)
import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.EmailAddress (EmailAddress)
import Effects.Observability qualified as Observability
import Log qualified
import OpenTelemetry.Trace qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML, RawHtml, parseFragment, readNodes, renderDocument)
import Text.XmlHtml qualified as Xml
import Text.XmlHtml.Optics

--------------------------------------------------------------------------------

type Route =
  "user"
    :> "login"
    :> Servant.Header "HX-Current-Url" Text
    :> Servant.QueryParam "redirect" Text
    :> Servant.QueryParam "email" EmailAddress
    :> Servant.Get '[HTML] RawHtml

--------------------------------------------------------------------------------

handler ::
  ( Applicative m,
    Has Trace.Tracer env,
    MonadCatch m,
    Log.MonadLog m,
    MonadUnliftIO m,
    MonadReader env m
  ) =>
  Maybe Text ->
  Maybe Text ->
  Maybe EmailAddress ->
  m RawHtml
handler hxCurrentUrl redirectQueryParam emailQueryParam =
  Observability.handlerSpan "GET /user/login" () display $ do
    pageFragment <- parseFragment $ template emailQueryParam $ hxCurrentUrl <|> redirectQueryParam
    page <- loadFrame pageFragment

    let html = renderDocument $ swapMain pageFragment page
    pure html

--------------------------------------------------------------------------------

swapMain :: [Xml.Node] -> Xml.Document -> Xml.Document
swapMain = swapInner _body

readUserAuthFragment :: (MonadIO m, Log.MonadLog m, MonadThrow m) => Auth.LoggedIn -> m [Xml.Node]
readUserAuthFragment = \case
  Auth.IsLoggedIn _ -> readNodes "src/Templates/Root/Logout/button.html"
  Auth.IsNotLoggedIn -> liftA2 (<>) (readNodes "src/Templates/Root/Login/button.html") (readNodes "src/Templates/Root/Register/button.html")
