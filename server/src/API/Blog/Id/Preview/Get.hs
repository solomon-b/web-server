module API.Blog.Id.Preview.Get where

--------------------------------------------------------------------------------

import API.Blog.Id.Template (contentFieldPreview)
import App.Auth qualified as Auth
import App.Errors (Unauthorized (..), throwErr)
import CMark qualified
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Data.Text.Display (display)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import Log qualified
import OpenTelemetry.Trace qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML, RawHtml, parseFragment, renderNodes)

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> "blog"
    :> Servant.Capture "id" BlogPosts.Id
    :> "preview"
    :> Servant.QueryParam "content" Text
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
  Auth.Authz ->
  BlogPosts.Id ->
  Maybe Text ->
  m RawHtml
handler (Auth.Authz User.Domain {..} _) bid content =
  Observability.handlerSpan "GET /blog/new/preview" () display $ do
    unless dIsAdmin $ throwErr Unauthorized
    pageFragment <- parseFragment $ contentFieldPreview bid $ fmap (CMark.commonmarkToHtml []) content
    pure $ renderNodes pageFragment
