module Utils.HTML where

--------------------------------------------------------------------------------

import Data.ByteString.Lazy as Lazy hiding (foldr)
import Data.Functor
import Data.Text (Text, unwords)
import Htmx.Lucid.Head
import Lucid
import Lucid.Base (makeAttributes)
import Network.HTTP.Media ((//), (/:))
import Servant

--------------------------------------------------------------------------------

data HTML = HTML

newtype RawHtml = RawHtml {unRaw :: Lazy.ByteString}

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

(\>) :: (Functor f) => (HtmlT f a -> HtmlT f a) -> HtmlT f a -> HtmlT f ()
(\>) a r = a r $> ()

htmlDoc :: (Monad f) => Text -> HtmlT f a -> HtmlT f a
htmlDoc title body =
  html_ $
    do
      head_ \> do
        meta_ [charset_ "utf-8"]
        title_ \> toHtml title
        useHtmxVersion (1, 9, 10)
      body_ body

classes_ :: [Text] -> Attributes
classes_ = makeAttributes "class" . Data.Text.unwords

toHTML :: Html a -> RawHtml
toHTML = RawHtml . renderBS
