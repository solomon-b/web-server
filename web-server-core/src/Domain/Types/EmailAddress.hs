module Domain.Types.EmailAddress
  ( EmailAddress,
    mkEmailAddress,
    ValidationFailure (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..))
import Data.Text.Encoding qualified as Text.Encoding
import GHC.Generics
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeValue (..), EncodeValue (..))
import Servant qualified
import Text.Email.Validate qualified as Validate

--------------------------------------------------------------------------------

newtype EmailAddress = EmailAddress {getEmailAddress :: Validate.EmailAddress}
  deriving stock (Show, Generic, Eq)

mkEmailAddress :: Text -> Either ValidationFailure EmailAddress
mkEmailAddress =
  fmap EmailAddress
    . first (const ValidationFailure)
    . Validate.validate
    . Text.Encoding.encodeUtf8
    . Text.toLower

emailAddressText :: EmailAddress -> Text
emailAddressText = Text.Encoding.decodeUtf8 . Validate.toByteString . getEmailAddress

data ValidationFailure = ValidationFailure
  deriving stock (Show, Eq)

instance Display ValidationFailure where
  displayBuilder _ = "Email ValidationFailure"

instance FromJSON EmailAddress where
  parseJSON = Aeson.withText "EmailAddress" $ \t ->
    case mkEmailAddress t of
      Left _ -> fail "Failed to parse EmailAddress"
      Right e -> pure e

instance ToJSON EmailAddress where
  toJSON = Aeson.String . emailAddressText

instance Display EmailAddress where
  displayBuilder = displayBuilder . emailAddressText

instance Servant.ToHttpApiData EmailAddress where
  toUrlPiece = emailAddressText

instance Servant.FromHttpApiData EmailAddress where
  parseUrlPiece = first (const "Failed to parse EmailAddress") . mkEmailAddress
  parseQueryParam = first (const "Failed to parse EmailAddress") . mkEmailAddress

instance EncodeValue EmailAddress where
  encodeValue = Encoders.enum emailAddressText

instance DecodeValue EmailAddress where
  decodeValue = Decoders.enum (either (const Nothing) Just . mkEmailAddress)
