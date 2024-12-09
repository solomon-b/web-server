module API.Image.Post where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Errors (Unauthorized (..), throwErr)
import Control.Monad (unless)
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
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display, RecordInstance (..), display)
import Data.Text.Encoding qualified as TE
import Deriving.Aeson qualified as Deriving
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.Images qualified as Images
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import GHC.Generics (Generic)
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import OrphanInstances.MultipartData (Extension (..), lookupFilePath)
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
    :> "image"
    :> "new"
    :> MultipartForm Tmp NewImage
    :> Servant.PostAccepted '[HTML] Servant.NoContent

--------------------------------------------------------------------------------

data NewImage = NewImage
  { title :: Text,
    filePath :: (Extension, FilePath)
  }
  deriving stock (Generic)
  deriving (Display) via (RecordInstance NewImage)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.CamelToSnake]] NewImage

instance FromMultipart Tmp NewImage where
  fromMultipart form = do
    title <- Multipart.lookupInput "title" form
    filePath <- lookupFilePath "filePath" form

    pure NewImage {..}

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
  NewImage ->
  m Servant.NoContent
handler (Auth.Authz User.Domain {dId = userId, dIsAdmin} _) NewImage {..} = do
  Observability.handlerSpan "POST /blog/new" () display $ do
    unless dIsAdmin $ throwErr Unauthorized
    filePath' <- copyFile filePath
    _ <- execQuerySpanThrow $ Images.insertImage $ Images.ModelInsert userId title (Text.pack filePath')
    pure Servant.NoContent

copyFile :: (MonadIO m) => (Extension, FilePath) -> m FilePath
copyFile (Extension ext, path) = liftIO $ do
  let name = encodeFilename path
      destination = "/static/images/" <> name <> Text.unpack ext
  Dir.copyFile path $ "." <> destination
  pure destination

encodeFilename :: FilePath -> FilePath
encodeFilename = Char8.unpack . Base64.extractBase64 . Base64.Url.encodeBase64' . SHA256.hash . TE.encodeUtf8 . Text.pack
