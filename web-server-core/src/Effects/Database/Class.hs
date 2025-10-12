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
import qualified Hasql.Transaction as HT
import qualified Hasql.Transaction.Sessions as HT

--------------------------------------------------------------------------------

class (MonadIO m) => MonadDB m where
  runDB :: Session a -> m (Either UsageError a)
  runDBTransaction :: HT.Transaction a -> m (Either UsageError a)

instance (Has HSQL.Pool env, MonadIO m) => MonadDB (ReaderT env m) where
  runDB :: Session a -> ReaderT env m (Either UsageError a)
  runDB s = do
    pool <- Reader.asks Has.getter
    liftIO $ HSQL.Pool.use pool s

  runDBTransaction :: HT.Transaction a -> ReaderT env m (Either UsageError a)
  runDBTransaction tx = do
    pool <- Reader.asks Has.getter
    liftIO $ HSQL.Pool.use pool (HT.transaction HT.Serializable HT.Write tx)


execStatement :: (MonadDB m) => Statement () a -> m (Either UsageError a)
execStatement = runDB . Hasql.statement ()

execTransaction :: (MonadDB m) => HT.Transaction a -> m (Either UsageError a)
execTransaction = runDBTransaction

healthCheck :: Statement () ()
healthCheck = interp False [sql|select current_timestamp|]
