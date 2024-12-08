module API.Image.Post where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Base64.Types qualified as Base64
import Data.ByteString.Base64.URL qualified as Base64.Url
import Data.ByteString.Char8 qualified as Char8
import Data.Foldable (forM_)
import Data.Has (Has)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Text.Encoding qualified as TE
import Effects.Database.Class (MonadDB)
import Effects.Observability qualified as Observability
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import OrphanInstances.MultipartData ()
import OrphanInstances.OneRow ()
import OrphanInstances.Servant ()
import Servant ((:>))
import Servant qualified
import Servant.Multipart (MultipartData, MultipartForm, Tmp)
import Servant.Multipart qualified as Multipart
import System.Directory qualified as Dir
import System.FilePath qualified as FilePath
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  "image"
    :> "new"
    :> MultipartForm Tmp (MultipartData Tmp)
    :> Servant.PostAccepted '[HTML] Servant.NoContent

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
  -- Auth.Authz ->
  MultipartData Tmp ->
  m Servant.NoContent
-- handler (Auth.Authz _ _) = do
handler multipartData = do
  Observability.handlerSpan "POST /blog/new" () display $ do
    forM_ (Multipart.files multipartData) copyFile
    pure Servant.NoContent

copyFile :: (MonadIO m) => Multipart.FileData Tmp -> m FilePath
copyFile file = liftIO $ do
  let path = takePath file
      ext = takeExtension file
      name = encodeFilename file
      destination = "./static/" <> name <> ext
  Dir.copyFile path destination
  pure destination

encodeFilename :: Multipart.FileData tag -> String
encodeFilename = Char8.unpack . Base64.extractBase64 . Base64.Url.encodeBase64' . SHA256.hash . TE.encodeUtf8 . Multipart.fdFileName

takeExtension :: Multipart.FileData tag -> String
takeExtension = FilePath.takeExtension . Text.unpack . Multipart.fdFileName

takePath :: Multipart.FileData Tmp -> String
takePath = Multipart.fdPayload
