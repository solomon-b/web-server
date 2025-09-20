{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OrphanInstances.NonEmptyText where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.NonEmptyText (NonEmptyText)
import Data.NonEmptyText qualified as NonEmptyText
import Data.Text.Display (Display (..))
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeValue (..), EncodeValue (..))
import Lucid qualified
import Servant qualified

--------------------------------------------------------------------------------

instance Aeson.ToJSON NonEmptyText where
  toJSON = Aeson.String . NonEmptyText.toText

instance Aeson.FromJSON NonEmptyText where
  parseJSON = Aeson.withText "Password" (maybe (fail "Failed to parse NonEmptyText") pure . NonEmptyText.fromText)

instance Display NonEmptyText where
  displayBuilder = displayBuilder . NonEmptyText.toText

instance DecodeValue NonEmptyText where
  decodeValue = Decoders.enum NonEmptyText.fromText

instance EncodeValue NonEmptyText where
  encodeValue = Encoders.enum NonEmptyText.toText

instance Servant.ToHttpApiData NonEmptyText where
  toUrlPiece = NonEmptyText.toText
  toQueryParam = NonEmptyText.toText

instance Servant.FromHttpApiData NonEmptyText where
  parseUrlPiece = maybe (Left "Failed to parse NonEmptyText") Right . NonEmptyText.fromText
  parseQueryParam = maybe (Left "Failed to parse NonEmptyText") Right . NonEmptyText.fromText

instance Lucid.ToHtml NonEmptyText where
  toHtml = Lucid.toHtml . NonEmptyText.toText
  toHtmlRaw = Lucid.toHtmlRaw . NonEmptyText.toText
