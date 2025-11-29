-- | General purpose utility functions
module Utils where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as Text
import Network.URI qualified as URI

--------------------------------------------------------------------------------

-- | Read and parse JSON data from disk.
readJSON :: (FromJSON a) => FilePath -> IO a
readJSON path' = do
  raw <- Aeson.eitherDecode <$> BL.readFile path'
  case raw of
    Left err -> error $ show err
    Right json -> pure json

combine :: (Applicative f) => (f a, f b) -> f (a, b)
combine (x, y) = liftA2 (,) x y

escapeString :: Text -> Text
escapeString = Text.pack . URI.escapeURIString URI.isUnreserved . Text.unpack

unEscapeString :: Text -> Text
unEscapeString = Text.pack . URI.unEscapeString . Text.unpack
