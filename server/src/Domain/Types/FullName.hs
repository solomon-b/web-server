module Domain.Types.FullName
  ( FullName,
    mkFullName,
    mkFullNameUnsafe,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.NonEmptyText (NonEmptyText)
import Data.NonEmptyText qualified as NonEmptyText
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeValue, EncodeValue)
import OrphanInstances.NonEmptyText ()
import Servant qualified

--------------------------------------------------------------------------------

newtype FullName = FullName {fullName :: NonEmptyText}
  deriving stock (Show, Generic, Eq)
  deriving newtype (Servant.ToHttpApiData, Servant.FromHttpApiData, FromJSON, ToJSON, Display, DecodeValue, EncodeValue)

mkFullName :: Text -> Maybe FullName
mkFullName = fmap FullName . NonEmptyText.fromText

mkFullNameUnsafe :: Text -> FullName
mkFullNameUnsafe txt = FullName $ NonEmptyText.new (Text.head txt) (Text.tail txt)
