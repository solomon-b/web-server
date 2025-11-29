module Domain.Types.DisplayName
  ( DisplayName,
    mkDisplayName,
    mkDisplayNameUnsafe,
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
import Lucid qualified
import OrphanInstances.NonEmptyText ()
import Servant qualified

--------------------------------------------------------------------------------

newtype DisplayName = DisplayName {displayName :: NonEmptyText}
  deriving stock (Show, Generic, Eq)
  deriving newtype (Servant.ToHttpApiData, Servant.FromHttpApiData, FromJSON, ToJSON, Display, DecodeValue, EncodeValue, Lucid.ToHtml)

mkDisplayName :: Text -> Maybe DisplayName
mkDisplayName = fmap DisplayName . NonEmptyText.fromText

mkDisplayNameUnsafe :: Text -> DisplayName
mkDisplayNameUnsafe txt = DisplayName $ NonEmptyText.new (Text.head txt) (Text.tail txt)
