module Domain.Types.Amount where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON (..), ToJSON)
import Data.Int (Int64)
import Data.Text.Display (Display (..))
import GHC.Generics
import Hasql.Interpolate (DecodeValue, EncodeValue)
import Servant qualified

--------------------------------------------------------------------------------

-- | An amount of currency in cents.
newtype Amount = Amount {getAmount :: Int64}
  deriving stock (Show, Generic, Eq)
  deriving newtype (DecodeValue, EncodeValue, Servant.FromHttpApiData, Servant.ToHttpApiData, FromJSON, ToJSON)

instance Display Amount where
  displayBuilder (Amount i) = displayBuilder @Double (realToFrac i / 100)
