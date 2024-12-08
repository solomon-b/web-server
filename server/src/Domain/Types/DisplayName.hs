module Domain.Types.DisplayName
  ( DisplayName,
    mkDisplayName,
    mkDisplayNameUnsafe,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Char qualified as Char
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeValue, EncodeValue)
import Servant qualified

--------------------------------------------------------------------------------

newtype DisplayName = DisplayName {displayName :: Text}
  deriving stock (Show, Generic, Eq)
  deriving newtype (Servant.FromHttpApiData, FromJSON, ToJSON, Display, DecodeValue, EncodeValue)

mkDisplayName :: Text -> Maybe DisplayName
mkDisplayName nm
  | Text.null nm = Nothing
  | Char.isSpace (Text.head nm) = Nothing
  | Char.isSpace (Text.last nm) = Nothing
  | otherwise = Just $ DisplayName nm

mkDisplayNameUnsafe :: Text -> DisplayName
mkDisplayNameUnsafe = DisplayName
