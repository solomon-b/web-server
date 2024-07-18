module Errors where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadThrow (..))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Servant.Server qualified as Servant

--------------------------------------------------------------------------------

toErrorBody :: Text -> Int -> BL.ByteString
toErrorBody msg errorCode = Aeson.encode $ Aeson.object ["error" .= Aeson.object ["message" .= msg, "code" .= errorCode]]

class ToServerError e where
  toServerError :: e -> Servant.ServerError

instance ToServerError Servant.ServerError where
  toServerError = id

throwErr :: (MonadThrow m, ToServerError e) => e -> m a
throwErr = throwM . toServerError

--------------------------------------------------------------------------------

data Unauthorized = Unauthorized

instance ToServerError Unauthorized where
  toServerError _ = Servant.err401

data Forbidden = Forbidden

instance ToServerError Forbidden where
  toServerError _ = Servant.err403

data InternalServerError = InternalServerError

instance ToServerError InternalServerError where
  toServerError _ = Servant.err500
