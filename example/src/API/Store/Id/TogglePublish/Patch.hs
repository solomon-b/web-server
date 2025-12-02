module API.Store.Id.TogglePublish.Patch where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Errors (NotFound (..), Unauthorized (..), throwErr)
import Control.Monad (unless, void)
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Display (display)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Queries.UserWithMetadata (FullUser (..))
import Effects.Database.Tables.Products qualified as Products
import Effects.Observability qualified as Observability
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import OrphanInstances.OneRow ()
import OrphanInstances.Servant ()
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> Servant.Header "HX-Current-URL" Text
    :> "blog"
    :> Servant.Capture "id" Products.Id
    :> "toggle-publish"
    :> Servant.PatchAccepted '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] Servant.NoContent)

--------------------------------------------------------------------------------

handler ::
  ( MonadReader env m,
    Has OTEL.Tracer env,
    Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  Auth.Authz ->
  Maybe Text ->
  Products.Id ->
  m
    ( Servant.Headers
        '[ Servant.Header "HX-Redirect" Text
         ]
        Servant.NoContent
    )
handler (Auth.Authz FullUser {fuIsAdmin} _) mReferer pid = do
  Observability.handlerSpan "PATCH /store/:id/toggle-publish" $ do
    unless fuIsAdmin $ throwErr Unauthorized
    void $ execQuerySpanThrow $ Products.togglePublished pid

    let redirectTo = fromMaybe "/store" mReferer
    pure $ Servant.addHeader redirectTo Servant.NoContent
