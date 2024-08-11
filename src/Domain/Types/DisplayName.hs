module Domain.Types.DisplayName where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text.Display (Display)
import GHC.Generics (Generic)
import Hasql.Interpolate (EncodeValue)
import Servant qualified

--------------------------------------------------------------------------------

newtype DisplayName = DisplayName {displayName :: Text}
  deriving stock (Show, Generic, Eq)
  deriving newtype (Servant.FromHttpApiData, FromJSON, ToJSON, Display, EncodeValue)
