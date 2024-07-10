module Errors where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadThrow (..))
import Data.ByteString.Lazy qualified as BL
import Network.HTTP.Types (Header)
import Servant.Server qualified as Servant

--------------------------------------------------------------------------------

throw301 :: (MonadThrow m) => BL.ByteString -> m a
throw301 err = throwM (Servant.err301 {Servant.errBody = err})

throw307 :: (MonadThrow m) => BL.ByteString -> [Header] -> m a
throw307 err headers = throwM (Servant.err307 {Servant.errBody = err, Servant.errHeaders = headers})

throw400 :: (MonadThrow m) => BL.ByteString -> m a
throw400 err = throwM (Servant.err400 {Servant.errBody = err})

throw401 :: (MonadThrow m) => BL.ByteString -> m a
throw401 err = throwM (Servant.err401 {Servant.errBody = err})

throw401' :: (MonadThrow m) => m a
throw401' = throwM Servant.err401

throw403 :: (MonadThrow m) => BL.ByteString -> m a
throw403 err = throwM (Servant.err403 {Servant.errBody = err})

throw403' :: (MonadThrow m) => m a
throw403' = throwM Servant.err403

throw500 :: (MonadThrow m) => BL.ByteString -> m a
throw500 err = throwM (Servant.err500 {Servant.errBody = err})

throw500' :: (MonadThrow m) => m a
throw500' = throwM Servant.err500
