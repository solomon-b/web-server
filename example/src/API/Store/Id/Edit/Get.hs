module API.Store.Id.Edit.Get where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Errors (NotFound (..), Unauthorized (..), throwErr)
import Component.Forms.Product (template)
import Component.Frame (loadFrameWithNav)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Display (display)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Queries.UserWithMetadata (FullUser (..))
import Effects.Database.Tables.Images qualified as Images
import Effects.Database.Tables.Products qualified as Products
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
    :> "store"
    :> Servant.Capture "id" Products.Id
    :> "edit"
    :> Servant.QueryParam "description" Text
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
  Products.Id ->
  Maybe Text ->
  m (Servant.Headers '[Servant.Header "Vary" Text] (Lucid.Html ()))
handler (Auth.Authz user@FullUser {fuIsAdmin} _) hxTrigger pid descriptionParam =
  Observability.handlerSpan "GET /store/new" () (display . Servant.getResponse) $ do
    Products.Domain {..} <- maybe (throwErr NotFound) (pure . Products.toDomain) =<< execQuerySpanThrow (Products.get pid)
    unless fuIsAdmin $ throwErr Unauthorized

    let description = fromMaybe dDescription descriptionParam
    let pageFragment = template (Just pid) (Just dName) (Just description) (fmap Images.dFilePath dHeroImage) (Just dPriceCents) (Just dCurrency) (Just dStockQuantity) dPublished

    case hxTrigger of
      Just True ->
        pure $ Servant.addHeader "HX-Request" pageFragment
      _ -> do
        fullPage <- loadFrameWithNav (Auth.IsLoggedIn user) "store-tab" pageFragment
        pure $ Servant.addHeader "HX-Request" fullPage
