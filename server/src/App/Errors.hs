module App.Errors where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadThrow (..))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy.Encoding qualified as TE
import Log qualified
import Servant.Server qualified as Servant

--------------------------------------------------------------------------------

toErrorBody :: Text -> Int -> BL.ByteString
toErrorBody msg errorCode = Aeson.encode $ Aeson.object ["error" .= Aeson.object ["message" .= msg, "code" .= errorCode]]

class ToServerError e where
  toServerError :: e -> Servant.ServerError
  toServerLog :: e -> (Text, Maybe Aeson.Value)

instance ToServerError Servant.ServerError where
  toServerError = id
  toServerLog err = (Text.pack (Servant.errReasonPhrase err), Just $ Aeson.object ["details" .= TE.decodeUtf8 (Servant.errBody err)])

throwErr :: (Log.MonadLog m, MonadThrow m, ToServerError e) => e -> m a
throwErr err = do
  uncurry Log.logAttention $ toServerLog err
  throwM $ toServerError err

--------------------------------------------------------------------------------

data Unauthorized = Unauthorized

instance ToServerError Unauthorized where
  toServerError _ = Servant.err401
  toServerLog _ = ("Unauthorized", Nothing)

data Forbidden = Forbidden

instance ToServerError Forbidden where
  toServerError _ = Servant.err403
  toServerLog _ = ("Forbidden", Nothing)

data NotFound = NotFound

instance ToServerError NotFound where
  toServerError _ = Servant.err404
  toServerLog _ = ("NotFound", Nothing)

newtype InternalServerError = InternalServerError Text

instance ToServerError InternalServerError where
  toServerError _ = Servant.err500
  toServerLog (InternalServerError msg) = ("InternalServerError", Just $ Aeson.object ["details" .= msg])
