module API.Blog.Id.Edit.Get where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Errors (NotFound (..), Unauthorized (..), throwErr)
import Component.Forms.BlogPost qualified as Forms.BlogPost
import Component.Frame (loadFrameWithNav)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.ByteString.Lazy qualified as BL
import Data.Has (Has)
import Data.Maybe (fromMaybe)
import Data.Text.Display (display)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.Images qualified as Images
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML, RawHtml, parseFragment, renderDocument)
import Text.XmlHtml qualified as Xml
import Text.XmlHtml.Optics

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> "blog"
    :> Servant.Capture "id" BlogPosts.Id
    :> "edit"
    :> Servant.QueryParam "content" BlogPosts.Body
    :> Servant.Get '[HTML] RawHtml

--------------------------------------------------------------------------------

handler ::
  ( Applicative m,
    Has Trace.Tracer env,
    Log.MonadLog m,
    MonadCatch m,
    MonadDB m,
    MonadUnliftIO m,
    MonadReader env m
  ) =>
  Auth.Authz ->
  BlogPosts.Id ->
  Maybe BlogPosts.Body ->
  m RawHtml
handler (Auth.Authz user@User.Domain {dId = uid, ..} _) bid contentParam =
  Observability.handlerSpan "GET /post/new" () display $ do
    BlogPosts.Domain {..} <- maybe (throwErr NotFound) (pure . BlogPosts.toDomain) =<< execQuerySpanThrow (BlogPosts.getBlogPost bid)
    unless (dIsAdmin || uid == dAuthorId) $ throwErr Unauthorized

    let content = fromMaybe dContent contentParam
    pageFragment <- parseFragment $ BL.toStrict $ Lucid.renderBS $ Forms.BlogPost.template (Just bid) (Just dTitle) (Just content) dPublished (fmap Images.dFilePath dHeroImage)
    page <- loadFrameWithNav (Auth.IsLoggedIn user) "blog-tab" pageFragment
    let html = renderDocument $ swapMain pageFragment page
    pure html

--------------------------------------------------------------------------------

swapMain :: [Xml.Node] -> Xml.Document -> Xml.Document
swapMain = swapInner _main
