module Text.HTML where

--------------------------------------------------------------------------------

import App.Errors (InternalServerError (..), throwErr)
import Control.Lens (preview, view)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Binary.Builder qualified as Builder
import Data.ByteString as BS
import Data.ByteString.Lazy as Lazy hiding (foldr)
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

printDocument :: Xml.Document -> IO ()
printDocument = printRawHtml . renderHTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

-- | Serialize an 'Xml.Document' into 'RawHtml'.
-- TODO: Rename to @renderDocument@
renderHTML :: Xml.Document -> RawHtml
renderHTML =
  RawHtml . Builder.toLazyByteString . Xml.render

-- | Serialize an 'Xml.Node' into 'RawHtml'.
renderNode :: Xml.Node -> RawHtml
renderNode =
  RawHtml . Builder.toLazyByteString . Xml.renderXmlFragment Xml.UTF8 . pure

-- | Serialize a list of 'Xml.Node' into 'RawHtml'.
-- TODO: rename to @renderNodes@
renderFragment :: [Xml.Node] -> RawHtml
renderFragment =
  RawHtml . Builder.toLazyByteString . Xml.renderXmlFragment Xml.UTF8

-- | Parses an HTML 'Xml.Document' from a 'BS.ByteString' or throws an
-- @InternalServerError@ in some Monadic context.
parseDocument' :: (MonadThrow m) => BS.ByteString -> m Xml.Document
parseDocument' = either (\_ -> throwErr InternalServerError) pure . Xml.parseHTML "index.html"

-- | Parses an HTML 'Xml.Document' from a 'BS.ByteString' or returns a
-- @Nothing@.
parseDocument :: BS.ByteString -> Maybe Xml.Document
parseDocument = either (const Nothing) Just . Xml.parseHTML "index.html"

-- | Parses a single 'Xml.Node' from a 'BS.ByteString'.
parseNode :: BS.ByteString -> Maybe Xml.Node
parseNode bs = parseDocument' bs >>= preview Xml.Optics._docContent'

-- | Parses a list of 'Xml.Node' from a 'BS.ByteString'.
parseFragment :: (MonadThrow m) => BS.ByteString -> m [Xml.Node]
parseFragment bs = view Xml.Optics._docContent <$> parseDocument' bs

-- | Parse an HTML5 document
readDocument :: (MonadIO m, MonadThrow m) => FilePath -> m Xml.Document
readDocument fp =
  liftIO (BS.readFile fp) >>= parseDocument'

-- | Parse an HTML5 document from disk
-- TODO: Rename to @parseNodes@
readFragment :: (MonadIO m, MonadThrow m) => FilePath -> m [Xml.Node]
readFragment path = view Xml.Optics._docContent <$> readDocument path
