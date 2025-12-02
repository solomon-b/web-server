module API.Store.New.Post where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (blogNewGetLink)
import App.Auth qualified as Auth
import App.Errors (Unauthorized (..), throwErr)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (FromJSON, ToJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Base64.Types qualified as Base64
import Data.ByteString.Base64.URL qualified as Base64.Url
import Data.ByteString.Char8 qualified as Char8
import Data.Has (Has)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..), RecordInstance (..), display)
import Data.Text.Encoding qualified as TE
import Data.Validation (Validation (..))
import Deriving.Aeson qualified as Deriving
import Domain.Types.Amount (Amount (..))
import Domain.Types.ProductName (ProductName, mkProductName)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Queries.UserWithMetadata (FullUser (..))
import Effects.Database.Tables.Images qualified as Images
import Effects.Database.Tables.Products qualified as Products
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import GHC.Generics (Generic)
import Hasql.Interpolate (OneRow (..))
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import OrphanInstances.MultipartData (Extension (..), lookupFilePathMaybe, readInput)
import OrphanInstances.OneRow ()
import OrphanInstances.Servant ()
import Servant ((:>))
import Servant qualified
import Servant.Multipart (FromMultipart, MultipartForm, Tmp)
import Servant.Multipart qualified as Multipart
import System.Directory qualified as Dir
import Text.HTML (HTML)
import Web.HttpApiData qualified as Http

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> "store"
    :> "new"
    :> MultipartForm Tmp CreateProduct
    :> Servant.PostAccepted '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] Servant.NoContent)

--------------------------------------------------------------------------------

data CreateProduct = CreateProduct
  { name :: ProductName,
    description :: Text,
    heroImagePath :: Maybe (Extension, FilePath),
    priceCents :: Amount,
    currency :: Text,
    stockQuantity :: Int64,
    active :: Bool
  }
  deriving stock (Generic)
  deriving (Display) via (RecordInstance CreateProduct)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.CamelToSnake]] CreateProduct

instance FromMultipart Tmp CreateProduct where
  fromMultipart form = do
    nameM <- mkProductName <$> Multipart.lookupInput "name" form
    description <- Multipart.lookupInput "description" form
    let heroImagePath = lookupFilePathMaybe "heroImagePath" form
    priceCents <- readInput "priceCents" form
    currency <- readInput "currency" form
    stockQuantity <- readInput "stockQuantity" form
    active <- readInput "active" form

    case nameM of
      Just name -> pure CreateProduct {..}
      Nothing -> Left "Name must not be empty"

-- TODO:
data ValidationError = MissingName | MissingDescription | NegativePrice | InvalidCurrency | NegativeStockQuantity

instance Display ValidationError where
  displayBuilder = \case
    MissingName -> "Name is missing."
    MissingDescription -> "Description is missing."
    NegativePrice -> "Price cannot be negative."
    InvalidCurrency -> "Currency is invalid."
    NegativeStockQuantity -> "StockQuantity cannot be negative."

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
  CreateProduct ->
  m
    ( Servant.Headers
        '[ Servant.Header "HX-Redirect" Text
         ]
        Servant.NoContent
    )
handler (Auth.Authz FullUser {fuId = userId, fuIsAdmin} _) req@CreateProduct {..} = do
  Observability.handlerSpan "POST /store/new" $ do
    unless fuIsAdmin $ throwErr Unauthorized
    case validateRequest req of
      Failure err -> do
        Log.logInfo "POST /store/new Request validation failure" (Aeson.object ["request" .= req, "validationErrors" .= display err])
        pure $ Servant.addHeader ("/" <> Http.toUrlPiece blogNewGetLink) Servant.NoContent
      Success () -> do
        heroImageId <- traverse (insertImage userId) heroImagePath
        bid <- execQuerySpanThrow $ Products.insert $ Products.Insert name description heroImageId priceCents currency stockQuantity active
        pure $ Servant.addHeader ("/store/" <> display bid) Servant.NoContent

--------------------------------------------------------------------------------

validateDescription :: Text -> Validation [ValidationError] ()
validateDescription description =
  if Text.null description then Failure [MissingDescription] else Success ()

validatePriceCents :: Amount -> Validation [ValidationError] ()
validatePriceCents (Amount price)
  | price >= 0 = pure ()
  | otherwise = Failure [NegativePrice]

validateCurrency :: Text -> Validation [ValidationError] ()
validateCurrency "USD" = pure ()
validateCurrency _ = Failure [InvalidCurrency]

validateStockQuantity :: Int64 -> Validation [ValidationError] ()
validateStockQuantity stockQuantity
  | stockQuantity >= 0 = pure ()
  | otherwise = Failure [NegativeStockQuantity]

validateRequest :: CreateProduct -> Validation [ValidationError] ()
validateRequest CreateProduct {..} =
  validateDescription description
    *> validatePriceCents priceCents
    *> validateCurrency currency
    *> validateStockQuantity stockQuantity

--------------------------------------------------------------------------------

insertImage ::
  ( MonadIO m,
    Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    MonadReader env m,
    Has OTEL.Tracer env,
    MonadUnliftIO m
  ) =>
  User.Id ->
  (Extension, FilePath) ->
  m Images.Id
insertImage uid fp = do
  path' <- copyFile fp
  OneRow imageId <- execQuerySpanThrow $ Images.insertImage $ Images.ModelInsert uid "Hero Image" path'
  pure imageId

copyFile :: (MonadIO m) => (Extension, FilePath) -> m Text
copyFile (Extension ext, path) = liftIO $ do
  let name = encodeFilename path
      destination = "/static/images/" <> name <> Text.unpack ext
  Dir.copyFile path $ "./server" <> destination
  pure $ Text.pack destination

encodeFilename :: FilePath -> FilePath
encodeFilename = Char8.unpack . Base64.extractBase64 . Base64.Url.encodeBase64' . SHA256.hash . TE.encodeUtf8 . Text.pack
