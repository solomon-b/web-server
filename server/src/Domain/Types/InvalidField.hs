module Domain.Types.InvalidField
  ( InvalidField,
    mkInvalidField,
    mkInvalidFieldUnsafe,
  )
where

--------------------------------------------------------------------------------

import Data.Char qualified as Char
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Servant qualified

--------------------------------------------------------------------------------

newtype InvalidField = InvalidField {invalidField :: Text}
  deriving stock (Show, Generic)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData, Eq, IsString, Semigroup, Monoid)

mkInvalidField :: Text -> Maybe InvalidField
mkInvalidField nm
  | Text.null nm = Nothing
  | Char.isSpace (Text.head nm) = Nothing
  | Char.isSpace (Text.last nm) = Nothing
  | otherwise = Just $ InvalidField nm

mkInvalidFieldUnsafe :: Text -> InvalidField
mkInvalidFieldUnsafe = InvalidField
