module Utils.HTML where

--------------------------------------------------------------------------------

import Control.Lens (view)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Binary.Builder qualified as Builder
import Data.ByteString as BS
import Data.ByteString.Lazy as Lazy hiding (foldr)
import Errors (InternalServerError (..), throwErr)
import Network.HTTP.Media ((//), (/:))
import Servant
import Text.XmlHtml qualified as Xml
import Text.XmlHtml.Optics qualified as Xml.Optics

--------------------------------------------------------------------------------

data HTML = HTML

newtype RawHtml = RawHtml {unRaw :: Lazy.ByteString}
  deriving newtype (Show)

printRawHtml :: RawHtml -> IO ()
printRawHtml = Lazy.putStr . unRaw

p = printRawHtml . renderHTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

renderHTML :: Xml.Document -> RawHtml
renderHTML =
  RawHtml . Builder.toLazyByteString . Xml.render

renderFragment :: [Xml.Node] -> RawHtml
renderFragment =
  RawHtml . Builder.toLazyByteString . Xml.renderXmlFragment Xml.UTF8

-- | Parse an HTML5 document
readDocument :: (MonadIO m, MonadThrow m) => FilePath -> m Xml.Document
readDocument fp =
  liftIO (BS.readFile fp) >>= parseDocument

parseDocument :: (MonadThrow m) => BS.ByteString -> m Xml.Document
parseDocument = either (\_ -> throwErr InternalServerError) pure . Xml.parseHTML "index.html"

-- | Parse an HTML5 document from disk
readFragment :: (MonadIO m, MonadThrow m) => FilePath -> m [Xml.Node]
readFragment path = view Xml.Optics._docContent <$> readDocument path

parseFragment :: (MonadThrow m) => BS.ByteString -> m [Xml.Node]
parseFragment bs = view Xml.Optics._docContent <$> parseDocument bs

err501Raw :: RawHtml
err501Raw = RawHtml "Oops!"
