module Effects.Database.Class where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO)
import Hasql.Pool (UsageError)
import Hasql.Session (Session)
import Hasql.Session qualified as Hasql
import Hasql.Statement (Statement)

--------------------------------------------------------------------------------

class (MonadIO m) => MonadDB m where
  runDB :: Session a -> m (Either UsageError a)

execStatement :: (MonadDB m) => Statement () a -> m (Either UsageError a)
execStatement = runDB . Hasql.statement ()
