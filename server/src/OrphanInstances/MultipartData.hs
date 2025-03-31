{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OrphanInstances.MultipartData where

--------------------------------------------------------------------------------

import Control.Arrow ((&&&))
import Control.Monad ((>=>))
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first)
import Data.Foldable
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.Display (Display)
import Data.Text.Display.Core (Display (..))
import Servant (FromHttpApiData)
import Servant.Multipart (MultipartData (..))
import Servant.Multipart qualified as Multipart
import System.FilePath qualified as FilePath
import Web.HttpApiData qualified as HTTP

--------------------------------------------------------------------------------

instance Display (MultipartData Multipart.Tmp) where
  displayBuilder MultipartData {..} =
    displayBuilder $
      fold
        [ "inputs: ",
          T.pack $ show inputs,
          "\n",
          "files: ",
          T.pack $ show files
        ]

instance Display (MultipartData Multipart.Mem) where
  displayBuilder MultipartData {..} =
    displayBuilder $
      fold
        [ "inputs: ",
          T.pack $ show inputs,
          "\n",
          "files: ",
          T.pack $ show files
        ]

-- | Parse an input using a 'Read' instance.
readInput :: forall tag a. (FromHttpApiData a) => Text -> MultipartData tag -> Either String a
readInput iname =
  Multipart.lookupInput iname >=> first Text.unpack . HTTP.parseQueryParam

-- | Optionally parse an input using a 'Read' instance.
readInputMaybe :: (FromHttpApiData a) => Text -> MultipartData tag -> Either String (Maybe a)
readInputMaybe iname =
  either (const (pure Nothing)) (pure . HTTP.parseQueryParamMaybe) . Multipart.lookupInput iname

-- | Lookup an optional textual input with the given @name@ attribute.
lookupInputMaybe :: Text -> MultipartData tag -> Maybe Text
lookupInputMaybe iname =
  fmap Multipart.iValue . find ((== iname) . Multipart.iName) . inputs

-- | Lookup an optional file input with the given @name@ attribute.
lookupFileMaybe :: Text -> MultipartData tag -> Maybe (Multipart.FileData tag)
lookupFileMaybe iname =
  find ((== iname) . Multipart.fdInputName) . files

-- | Lookup the 'FilePath' for a file input with the given @name@ attribute.
lookupFilePath :: Text -> MultipartData Multipart.Tmp -> Either String (Extension, FilePath)
lookupFilePath iname =
  fmap (Extension . Text.pack . FilePath.takeExtension . Text.unpack . Multipart.fdFileName &&& Multipart.fdPayload) . Multipart.lookupFile iname

newtype Extension = Extension {getExtension :: Text}
  deriving newtype (FromJSON, ToJSON, Display)

-- | Optionally lookup the 'FilePath' for a file input with the given @name@ attribute.
lookupFilePathMaybe :: Text -> MultipartData Multipart.Tmp -> Maybe (Extension, FilePath)
lookupFilePathMaybe iname =
  fmap (Extension . Text.pack . FilePath.takeExtension . Text.unpack . Multipart.fdFileName &&& Multipart.fdPayload) . lookupFileMaybe iname
