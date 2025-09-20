-- TODO: Switch to Text.Email.Validate.EmailAddress datatype.
module Domain.Types.EmailAddress
  ( EmailAddress,
    mkEmailAddress,
    isValid,
    ValidationFailure (..),
    validate,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON (..), ToJSON)
import Data.Aeson qualified as Aeson
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Text (Text)
import Data.Text.Display (Display (..))
import Data.Text.Encoding qualified as Text.Encoding
import GHC.Generics
import Hasql.Interpolate (DecodeValue, EncodeValue)
import OrphanInstances.CaseInsensitive ()
import Servant qualified
import Text.Email.Validate qualified as Validate

--------------------------------------------------------------------------------

newtype EmailAddress = EmailAddress {emailAddress :: CI Text}
  deriving stock (Show, Generic, Eq)
  deriving newtype (DecodeValue, EncodeValue)

instance FromJSON EmailAddress where
  parseJSON = fmap (EmailAddress . CI.mk) . Aeson.parseJSON @Text

instance ToJSON EmailAddress where
  toJSON EmailAddress {..} = Aeson.String $ CI.original emailAddress

instance Display EmailAddress where
  displayBuilder EmailAddress {..} = displayBuilder (CI.original emailAddress)

instance Servant.ToHttpApiData EmailAddress where
  toUrlPiece = CI.original . emailAddress

instance Servant.FromHttpApiData EmailAddress where
  parseUrlPiece = Right . EmailAddress . CI.mk
  parseQueryParam = Right . EmailAddress . CI.mk

mkEmailAddress :: Text -> EmailAddress
mkEmailAddress = EmailAddress . CI.mk

isValid :: EmailAddress -> Bool
isValid = Validate.isValid . Text.Encoding.encodeUtf8 . CI.original . emailAddress

data ValidationFailure = ValidationFailure

instance Display ValidationFailure where
  displayBuilder _ = "Email ValidationFailure"

-- TODO: Parse Dont Validate.
validate :: EmailAddress -> Either ValidationFailure EmailAddress
validate em =
  if isValid em
    then Right em
    else Left ValidationFailure
