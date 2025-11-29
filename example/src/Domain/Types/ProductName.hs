module Domain.Types.ProductName
  ( ProductName (..),
    mkProductName,
    mkProductNameUnsafe,
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

newtype ProductName = ProductName {displayName :: NonEmptyText}
  deriving stock (Show, Generic, Eq)
  deriving newtype (Servant.ToHttpApiData, Servant.FromHttpApiData, FromJSON, ToJSON, Display, DecodeValue, EncodeValue, Lucid.ToHtml)

mkProductName :: Text -> Maybe ProductName
mkProductName = fmap ProductName . NonEmptyText.fromText

mkProductNameUnsafe :: Text -> ProductName
mkProductNameUnsafe txt = ProductName $ NonEmptyText.new (Text.head txt) (Text.tail txt)
