{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.HTML
  ( HTML (..),
    renderNodes,
    parseFragment,
  )
where

--------------------------------------------------------------------------------

import App.Errors (InternalServerError (..), throwErr)
import Control.Monad.Catch (MonadThrow)
import Data.Binary.Builder qualified as Builder
import Data.ByteString as BS
import Data.Text qualified as Text
import Data.Text.Display (Display (..))
import Log qualified
import Lucid.Base qualified as Lucid
import Network.HTTP.Media ((//), (/:))
import Servant
import Text.XmlHtml qualified as Xml

--------------------------------------------------------------------------------

data HTML = HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML (Lucid.Html ()) where
  mimeRender _ = Lucid.renderBS

instance Display (Lucid.Html ()) where
  displayBuilder html = displayBuilder $ Lucid.renderText html

--------------------------------------------------------------------------------

-- | Serialize a list of 'Xml.Node' into 'RawHtml'.
renderNodes :: [Xml.Node] -> Lucid.Html ()
renderNodes =
  Lucid.toHtmlRaw . Builder.toLazyByteString . Xml.renderHtmlFragment Xml.UTF8

--------------------------------------------------------------------------------

-- | Parses an HTML 'Xml.Document' from a 'BS.ByteString' or throws an
-- @InternalServerError@ in some Monadic context.
parseDocument :: (Log.MonadLog m, MonadThrow m) => BS.ByteString -> m Xml.Document
parseDocument = either (throwErr . InternalServerError . Text.pack) pure . Xml.parseHTML "index.html"

-- | Parses a list of 'Xml.Node' from a 'BS.ByteString'.
parseFragment :: (Log.MonadLog m, MonadThrow m) => BS.ByteString -> m [Xml.Node]
parseFragment bs = Xml.docContent <$> parseDocument bs
