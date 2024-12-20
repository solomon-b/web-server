module Domain.Types.FullName
  ( FullName,
    mkFullName,
    mkFullNameUnsafe,
    Domain.Types.FullName.null,
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

newtype FullName = FullName {fullName :: Text}
  deriving stock (Show, Generic, Eq)
  deriving newtype (Servant.ToHttpApiData, Servant.FromHttpApiData, FromJSON, ToJSON, Display, DecodeValue, EncodeValue)

mkFullName :: Text -> Maybe FullName
mkFullName nm
  | Text.null nm = Nothing
  | Char.isSpace (Text.head nm) = Nothing
  | Char.isSpace (Text.last nm) = Nothing
  | otherwise = Just $ FullName nm

mkFullNameUnsafe :: Text -> FullName
mkFullNameUnsafe = FullName

null :: FullName -> Bool
null FullName {..} = Text.null fullName
