module Effects.Database.SerializedStatement where

import Data.ByteString.Char8 qualified as Char8
import Data.Text (Text)
import Data.Text qualified as Text
import Hasql.Encoders.All (Params (..))
import Hasql.Encoders.Params (renderReadable)
import Hasql.Statement (Statement (..))
import Hasql.Statement qualified as Hasql

--------------------------------------------------------------------------------

data SerializedStatement = SerializedStatement
  { sql :: Text,
    params :: [Text]
  }

serializeStatement :: a -> Hasql.Statement a b -> SerializedStatement
serializeStatement a (Statement q (Params p) _ _) =
  SerializedStatement
    { sql = Text.pack $ Char8.unpack q,
      params = renderReadable p a
    }
