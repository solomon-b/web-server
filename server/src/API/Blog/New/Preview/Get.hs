module API.Blog.New.Preview.Get where

--------------------------------------------------------------------------------

import API.Blog.New.Template
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
    :> "new"
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
  Maybe Text ->
  m RawHtml
handler (Auth.Authz User.Domain {..} _) content =
  Observability.handlerSpan "GET /blog/new/preview" () display $ do
    unless dIsAdmin $ throwErr Unauthorized
    pageFragment <- parseFragment $ contentFieldPreview $ fmap (CMark.commonmarkToHtml []) content
    pure $ renderNodes pageFragment
