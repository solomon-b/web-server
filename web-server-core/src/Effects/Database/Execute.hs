module Effects.Database.Execute where

--------------------------------------------------------------------------------

import App.Errors (InternalServerError (..), throwErr)
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
import Effects.Database.Class
import Effects.Database.SerializedStatement (SerializedStatement (..), serializeStatement)
import GHC.Num.Natural (Natural)
import Hasql.Pool qualified as HSQL
import Hasql.Session (SessionError (..))
import Hasql.Statement qualified as HSQL
import Log qualified
import OpenTelemetry.Attributes (Attribute)
import OpenTelemetry.Trace.Core qualified as Trace
import Servant qualified

--------------------------------------------------------------------------------
-- Query Execution

execQuerySpan ::
  ( Log.MonadLog m,
    MonadDB m,
    Reader.MonadReader env m,
    Has.Has Trace.Tracer env,
    MonadUnliftIO m,
    Display result
  ) =>
  HSQL.Statement () result ->
  m (Either HSQL.UsageError result)
execQuerySpan statement@(HSQL.Statement bs _ _ _) = do
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

    eQueryResult <- execStatement statement

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

execQuerySpanThrow ::
  ( Log.MonadLog m,
    Show result,
    MonadDB m,
    MonadThrow m,
    Reader.MonadReader env m,
    Has.Has Trace.Tracer env,
    MonadUnliftIO m,
    Display result
  ) =>
  HSQL.Statement () result ->
  m result
execQuerySpanThrow statement = do
  execQuerySpan statement >>= \case
    Left err -> do
      Log.logAttention "Query Execution Error" (show err)
      throwErr $ InternalServerError $ Text.pack $ show err
    Right res -> pure res

execQuerySpanThrowMessage ::
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
execQuerySpanThrowMessage msg statement = do
  execQuerySpan statement >>= \case
    Left err -> do
      Log.logAttention "Query Execution Error" (show err)
      throwErr Servant.err500 {Servant.errBody = msg}
    Right res -> pure res