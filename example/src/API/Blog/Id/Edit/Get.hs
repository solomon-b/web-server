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
import Data.Has (Has)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text.Display (display)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Queries.UserWithMetadata (FullUser (..))
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.Images qualified as Images
import Effects.Observability qualified as Observability
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> Servant.Header "HX-Request" Bool
    :> "blog"
    :> Servant.Capture "id" BlogPosts.Id
    :> "edit"
    :> Servant.QueryParam "content" BlogPosts.Body
    :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "Vary" Text] (Lucid.Html ()))

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
  Maybe Bool ->
  BlogPosts.Id ->
  Maybe BlogPosts.Body ->
  m (Servant.Headers '[Servant.Header "Vary" Text] (Lucid.Html ()))
handler (Auth.Authz user@FullUser {fuId = uid, ..} _) hxTrigger bid contentParam =
  Observability.handlerSpan "GET /post/new" () (display . Servant.getResponse) $ do
    BlogPosts.Domain {..} <- maybe (throwErr NotFound) (pure . BlogPosts.toDomain) =<< execQuerySpanThrow (BlogPosts.getBlogPost bid)
    unless (fuIsAdmin || uid == dAuthorId) $ throwErr Unauthorized

    let content = fromMaybe dContent contentParam
    let pageFragment = Forms.BlogPost.template (Just bid) (Just dTitle) (Just content) (isJust dPublishedAt) (fmap Images.dFilePath dHeroImage)

    case hxTrigger of
      Just True ->
        pure $ Servant.addHeader "HX-Request" pageFragment
      _ -> do
        fullPage <- loadFrameWithNav (Auth.IsLoggedIn user) "blog-tab" pageFragment
        pure $ Servant.addHeader "HX-Request" fullPage
