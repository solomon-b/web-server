module API.Store.Id.Edit.Post where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Errors (NotFound (..), Unauthorized (..), throwErr)
import Control.Applicative ((<|>))
import Control.Monad (unless, void)
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (FromJSON, ToJSON)
import Data.Base64.Types qualified as Base64
import Data.ByteString.Base64.URL qualified as Base64.Url
import Data.ByteString.Char8 qualified as Char8
import Data.Has (Has)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..), RecordInstance (..), display)
import Data.Text.Encoding qualified as TE
import Deriving.Aeson qualified as Deriving
import Domain.Types.Amount (Amount)
import Domain.Types.ProductName (ProductName (..), mkProductName)
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

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> "store"
    :> Servant.Capture "id" Products.Id
    :> "edit"
    :> MultipartForm Tmp EditProduct
    :> Servant.PostAccepted '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] Servant.NoContent)

--------------------------------------------------------------------------------

data EditProduct = EditProduct
  { name :: ProductName,
    description :: Text,
    heroImagePath :: Maybe (Extension, FilePath),
    priceCents :: Amount,
    currency :: Text,
    stockQuantity :: Int64,
    published :: Bool
  }
  deriving stock (Generic)
  deriving (Display) via (RecordInstance EditProduct)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.CamelToSnake]] EditProduct

instance FromMultipart Tmp EditProduct where
  fromMultipart :: Multipart.MultipartData Tmp -> Either String EditProduct
  fromMultipart form = do
    name <- maybe (Left "Invalid Display Name.") Right . mkProductName =<< Multipart.lookupInput "name" form
    description <- Multipart.lookupInput "description" form
    let heroImagePath = lookupFilePathMaybe "heroImagePath" form
    priceCents <- readInput "priceCents" form
    currency <- readInput "currency" form
    stockQuantity <- readInput "stockQuantity" form
    published <- readInput "published" form

    pure EditProduct {..}

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
  Products.Id ->
  EditProduct ->
  m
    ( Servant.Headers
        '[ Servant.Header "HX-Redirect" Text
         ]
        Servant.NoContent
    )
handler (Auth.Authz FullUser {fuId = userId, fuIsAdmin} _) pid req@EditProduct {..} = do
  Observability.handlerSpan "POST /store/:id/edit" $ do
    Products.Domain {dHeroImage} <- maybe (throwErr NotFound) (pure . Products.toDomain) =<< execQuerySpanThrow (Products.get pid)
    unless fuIsAdmin $ throwErr Unauthorized
    let oldHeroImageId = fmap Images.dId dHeroImage
    heroImageId <- traverse (insertImage userId) heroImagePath
    void $ execQuerySpanThrow $ Products.update $ Products.Model pid name description (heroImageId <|> oldHeroImageId) priceCents currency stockQuantity published
    pure $ Servant.addHeader ("/store/" <> display pid) Servant.NoContent

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
