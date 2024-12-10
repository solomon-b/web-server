{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Class where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader qualified as Reader
import Data.Has (Has)
import Data.Has qualified as Has
import Hasql.Decoders ()
import Hasql.Interpolate (interp, sql)
import Hasql.Pool (UsageError)
import Hasql.Pool qualified as HSQL
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Session (Session)
import Hasql.Session qualified as Hasql
import Hasql.Statement (Statement)

--------------------------------------------------------------------------------

class (MonadIO m) => MonadDB m where
  runDB :: Session a -> m (Either UsageError a)

instance (Has HSQL.Pool env, MonadIO m) => MonadDB (ReaderT env m) where
  runDB :: Session a -> ReaderT env m (Either UsageError a)
  runDB s = do
    pool <- Reader.asks Has.getter
    liftIO $ HSQL.Pool.use pool s

execStatement :: (MonadDB m) => Statement () a -> m (Either UsageError a)
execStatement = runDB . Hasql.statement ()

healthCheck :: Statement () ()
healthCheck = interp False [sql|select current_timestamp|]
