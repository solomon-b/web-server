module Effects.Database.Execute
  ( -- * Query Execution
    execQuery,
    execQueryThrow,
    execQueryThrowMessage,
    execTransaction,
    execTransactionThrow,

    -- * Utilities
    SerializedStatement (..),
    serializeStatement,
  )
where

--------------------------------------------------------------------------------

import App.Errors (InternalServerError (..), throwErr)
import Control.Monad.Catch (MonadThrow (..))
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as Text
import Data.Text.Display (Display)
import Data.Text.Encoding qualified as TE
import Effects.Database.Class
import Effects.Database.SerializedStatement (SerializedStatement (..), serializeStatement)
import qualified Hasql.Pool as HSQL
import Hasql.Statement qualified as HSQL
import Hasql.Transaction qualified as HT
import Log qualified
import Servant qualified

--------------------------------------------------------------------------------
-- Query Execution

execQuery ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    Display result
  ) =>
  HSQL.Statement () result ->
  m (Either HSQL.UsageError result)
execQuery statement@(HSQL.Statement bs _ _ _) = do
  Log.logInfo "db query" (TE.decodeUtf8 bs)
  execStatement statement

execQueryThrow ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    Display result
  ) =>
  HSQL.Statement () result ->
  m result
execQueryThrow statement@(HSQL.Statement bs _ _ _) = do
  Log.logInfo "db query" (TE.decodeUtf8 bs)
  execStatement statement >>= \case
    Left err -> throwErr $ InternalServerError $ Text.pack $ show err
    Right res -> pure res

execQueryThrowMessage ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    Display result
  ) =>
  BL.ByteString ->
  HSQL.Statement () result ->
  m result
execQueryThrowMessage msg statement@(HSQL.Statement bs _ _ _) = do
  Log.logInfo "db query" (TE.decodeUtf8 bs)
  execStatement statement >>= \case
    Left err -> do
      Log.logAttention "Query Execution Error" (show err)
      throwErr Servant.err500 {Servant.errBody = msg}
    Right res -> pure res

execTransactionThrow ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    Display result
  ) =>
  HT.Transaction result ->
  m result
execTransactionThrow transaction = do
  Log.logInfo "db transaction" ("running transaction" :: Text.Text)
  execTransaction transaction >>= \case
    Left err -> throwErr $ InternalServerError $ Text.pack $ show err
    Right res -> pure res
