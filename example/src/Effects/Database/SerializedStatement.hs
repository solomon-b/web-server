module Effects.Database.SerializedStatement where

import Data.ByteString.Char8 qualified as Char8
import Data.Text (Text)
import Data.Text qualified as Text
import Hasql.Statement (Statement (..))
import Hasql.Statement qualified as Hasql

--------------------------------------------------------------------------------

data SerializedStatement = SerializedStatement
  { sql :: Text,
    params :: [Text]
  }

serializeStatement :: a -> Hasql.Statement a b -> SerializedStatement
serializeStatement a (Statement q _ _ _) =
  SerializedStatement
    { sql = Text.pack $ Char8.unpack q,
      -- TODO: Figure out how to log display params without forking the library
      params = []
    }
