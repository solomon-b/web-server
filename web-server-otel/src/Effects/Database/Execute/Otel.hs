module Effects.Database.Execute.Otel
  ( -- * Traced Query Execution
    execQuery,
    execQueryThrow,
    execQueryThrowMessage,
    execTransaction,
    execTransactionThrow,

    -- * Utilities
    toStatementAttributes,
  )
where

--------------------------------------------------------------------------------

import App.Errors (InternalServerError (..), ToServerError, throwErr)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader qualified as Reader
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Has qualified as Has
import Data.HashMap.Strict qualified as HMS
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display, display)
import Data.Text.Encoding qualified as TE
import Effects.Database.Class (MonadDB)
import Effects.Database.Class qualified as DB
import Effects.Database.SerializedStatement (SerializedStatement (..), serializeStatement)
import GHC.Num.Natural (Natural)
import Hasql.Pool qualified as HSQL
import Hasql.Session (SessionError (..))
import Hasql.Statement qualified as HSQL
import Hasql.Transaction qualified as HT
import Log qualified
import OpenTelemetry.Attributes (Attribute)
import OpenTelemetry.Trace.Core qualified as Trace
import Servant qualified

--------------------------------------------------------------------------------
-- Query Execution

execTransaction ::
  ( Log.MonadLog m,
    MonadDB m,
    Reader.MonadReader env m,
    Has.Has Trace.Tracer env,
    MonadUnliftIO m,
    Display result
  ) =>
  HT.Transaction result ->
  m (Either HSQL.UsageError result)
execTransaction transaction = do
  tracer <- Reader.asks Has.getter
  Trace.inSpan' tracer "database_transaction" Trace.defaultSpanArguments $ \reqSpan -> do
    Log.logInfo "db transaction" $ KeyMap.singleton "type" ("transaction" :: Text)

    Trace.addEvent reqSpan $
      Trace.NewEvent
        { newEventName = "transaction_start",
          newEventAttributes = HMS.fromList [("isolation", Trace.toAttribute ("serializable" :: Text))],
          newEventTimestamp = Nothing
        }

    eQueryResult <- DB.execTransaction transaction

    case eQueryResult of
      Left (HSQL.SessionUsageError (QueryError _ params cmdError)) ->
        Trace.addEvent reqSpan $
          Trace.NewEvent
            { newEventName = "logical_query_error",
              newEventAttributes =
                HMS.fromList
                  [ ("cmd-error", Trace.toAttribute . Text.pack . show $ cmdError),
                    ("params", Trace.toAttribute . fold . intersperse " " $ params)
                  ],
              newEventTimestamp = Nothing
            }
      Left err ->
        Trace.addEvent reqSpan $
          Trace.NewEvent
            { newEventName = "unexpected_query_error",
              newEventAttributes = HMS.fromList [("error", Trace.toAttribute . Text.pack . show $ err)],
              newEventTimestamp = Nothing
            }
      Right result ->
        Trace.addEvent reqSpan $
          Trace.NewEvent
            { newEventName = "execution_result",
              newEventAttributes = HMS.fromList [("result", Trace.toAttribute . display $ result)],
              newEventTimestamp = Nothing
            }

    pure eQueryResult

execQuery ::
  ( Log.MonadLog m,
    MonadDB m,
    Reader.MonadReader env m,
    Has.Has Trace.Tracer env,
    MonadUnliftIO m,
    Display result
  ) =>
  HSQL.Statement () result ->
  m (Either HSQL.UsageError result)
execQuery statement@(HSQL.Statement bs _ _ _) = do
  tracer <- Reader.asks Has.getter
  Trace.inSpan' tracer "database_session" Trace.defaultSpanArguments $ \reqSpan -> do
    Log.logInfo "db query" $ KeyMap.singleton "query" (TE.decodeUtf8 bs)
    Trace.addEvent reqSpan $
      Trace.NewEvent
        { newEventName = "statement",
          newEventAttributes =
            HMS.fromList . toStatementAttributes $
              serializeStatement () statement,
          newEventTimestamp = Nothing
        }

    eQueryResult <- DB.execStatement statement

    case eQueryResult of
      Left (HSQL.SessionUsageError (QueryError _ params cmdError)) ->
        Trace.addEvent reqSpan $
          Trace.NewEvent
            { newEventName = "logical_query_error",
              newEventAttributes =
                HMS.fromList
                  [ ("cmd-error", Trace.toAttribute . Text.pack . show $ cmdError),
                    ("params", Trace.toAttribute . fold . intersperse " " $ params)
                  ],
              newEventTimestamp = Nothing
            }
      Left err ->
        Trace.addEvent reqSpan $
          Trace.NewEvent
            { newEventName = "unexpected_query_error",
              newEventAttributes = HMS.fromList [("error", Trace.toAttribute . Text.pack . show $ err)],
              newEventTimestamp = Nothing
            }
      Right result ->
        Trace.addEvent reqSpan $
          Trace.NewEvent
            { newEventName = "execution_result",
              newEventAttributes = HMS.fromList [("result", Trace.toAttribute . display $ result)],
              newEventTimestamp = Nothing
            }
    pure eQueryResult

toStatementAttributes :: SerializedStatement -> [(Text, Attribute)]
toStatementAttributes ss =
  let params' =
        zip @Natural [1 ..] (params ss)
          & foldMap
            ( \(n, p') ->
                mconcat
                  [ "\n  $",
                    Text.pack . show $ n,
                    " = ",
                    p'
                  ]
            )
   in [("sql", Trace.toAttribute (sql ss)), ("params", Trace.toAttribute params')]

-- | Helper to convert a database result to throw on error.
-- Note: throwErr already logs the error, so we don't need additional logging.
orThrowErr ::
  (Log.MonadLog m, MonadThrow m, ToServerError e) =>
  m (Either HSQL.UsageError a) ->
  (HSQL.UsageError -> e) ->
  m a
orThrowErr action mkErr = action >>= either (throwErr . mkErr) pure

execQueryThrow ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    Reader.MonadReader env m,
    Has.Has Trace.Tracer env,
    MonadUnliftIO m,
    Display result
  ) =>
  HSQL.Statement () result ->
  m result
execQueryThrow statement =
  execQuery statement `orThrowErr` (InternalServerError . Text.pack . show)

execQueryThrowMessage ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    Reader.MonadReader env m,
    Has.Has Trace.Tracer env,
    MonadUnliftIO m,
    Display result
  ) =>
  BL.ByteString ->
  HSQL.Statement () result ->
  m result
execQueryThrowMessage msg statement = do
  execQuery statement >>= \case
    Left err -> do
      -- Log the actual error for debugging, but throw a custom user message
      Log.logAttention "Query Execution Error" (show err)
      throwErr Servant.err500 {Servant.errBody = msg}
    Right res -> pure res

execTransactionThrow ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    Reader.MonadReader env m,
    Has.Has Trace.Tracer env,
    MonadUnliftIO m,
    Display result
  ) =>
  HT.Transaction result ->
  m result
execTransactionThrow transaction =
  execTransaction transaction `orThrowErr` (InternalServerError . Text.pack . show)
