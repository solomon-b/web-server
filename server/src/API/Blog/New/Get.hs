module API.Blog.New.Get where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Errors (Unauthorized (..), throwErr)
import Component.Frame (loadFrameWithNav)
import Component.Forms.BlogPost (template)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.InvalidField (InvalidField)
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import Log qualified
import OpenTelemetry.Trace qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML, RawHtml, parseFragment, renderDocument, renderNodes)
import Text.XmlHtml qualified as Xml
import Text.XmlHtml.Optics

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> Servant.Header "HX-Request" Bool
    :> "blog"
    :> "new"
    :> Servant.QueryParams "invalidField" InvalidField
    :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "Vary" Text] RawHtml)

--------------------------------------------------------------------------------

handler ::
  ( Applicative m,
    Has Trace.Tracer env,
    MonadCatch m,
    Log.MonadLog m,
    MonadUnliftIO m,
    MonadReader env m
  ) =>
  Auth.Authz ->
  Maybe Bool ->
  [InvalidField] ->
  m (Servant.Headers '[Servant.Header "Vary" Text] RawHtml)
handler (Auth.Authz user@User.Domain {..} _) hxTrigger invalidField =
  Observability.handlerSpan "GET /post/new" () (display . Servant.getResponse) $ do
    unless dIsAdmin $ throwErr Unauthorized

    pageFragment <- parseFragment (template Nothing Nothing Nothing False Nothing invalidField)
    page <- loadFrameWithNav (Auth.IsLoggedIn user) "blog-tab" pageFragment

    case hxTrigger of
      Just True ->
        pure $ Servant.addHeader "HX-Request" $ renderNodes pageFragment
      _ -> do
        let html = renderDocument $ swapMain pageFragment page
        pure $ Servant.addHeader "HX-Request" html

--------------------------------------------------------------------------------

swapMain :: [Xml.Node] -> Xml.Document -> Xml.Document
swapMain = swapInner _main
