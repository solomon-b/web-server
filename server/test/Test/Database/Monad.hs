{-# LANGUAGE TypeFamilies #-}

module Test.Database.Monad
  ( ReadOnly,
    WriteStatus (..),
    writeOnce,
    readOnlyTestDBConfig,
    withTestDB,
    withAuth,
    bracketConn,
    dbit,
    withTracer,
    TestDBInitConfig (..),
    TestDBConfig (..),
    TestDB (..),
    TestSetupException (..),
  )
where

--------------------------------------------------------------------------------

import App.Auth (Authz (..))
import Control.Concurrent (MVar, newEmptyMVar)
import Control.Concurrent.MVar (readMVar, tryPutMVar)
import Control.Exception (Exception, throw, throwIO)
import Control.Monad (void, when)
import Control.Monad.Catch (MonadCatch, bracket)
import Control.Monad.Catch.Pure (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Data.Bifunctor (first)
import Data.ByteString (ByteString, toStrict)
import Data.Foldable
import Data.Has qualified as Has
import Data.Maybe (fromMaybe)
import Data.Password.Argon2
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Domain.Types.DisplayName (mkDisplayNameUnsafe)
import Domain.Types.EmailAddress (mkEmailAddress)
import Domain.Types.FullName (mkFullNameUnsafe)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ServerSessions qualified as Session
import Effects.Database.Tables.User qualified as User
import GHC.IO (unsafePerformIO)
import GHC.IO.Exception (ExitCode (..))
import Hasql.Connection (Connection, Settings, acquire, release)
import Hasql.Interpolate (getOneRow)
import Hasql.Pool (UsageError (..))
import Hasql.Session (Session, run)
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Log qualified
import Log.Internal.Logger qualified as Log.Internal
import OpenTelemetry.Exporter.Span (ExportResult (..), SpanExporter (..))
import OpenTelemetry.Processor.Simple (SimpleProcessorConfig (..), simpleProcessor)
import OpenTelemetry.Trace qualified as OTEL
import System.Process.Typed (proc, readProcess)
import Test.Hspec (Spec, SpecWith, it)
import Test.Hspec.Core.Hooks (around, beforeWith)
import Test.Hspec.Core.Spec (ActionWith, Example (..), Params, ProgressCallback, Result)

--------------------------------------------------------------------------------

-- FIXME: Break up into multiple files & create folder hierarchy for top level files
--
-- add test db monad
--
-- add example instance
-- - https://github.com/hspec/hspec/blob/main/hspec-core/src/Test/Hspec/Core/Example.hs
-- - https://hackage.haskell.org/package/yesod-test-1.6.15/docs/src/Yesod.Test.html#local-6989586621679149801

newtype ReadOnly a = ReadOnly {untouchable :: MVar a}

onlyRead :: ReadOnly a -> IO a
onlyRead = readMVar . untouchable

data WriteStatus = AlreadyWritten | WriteSuccess

writeOnce :: ReadOnly a -> a -> IO WriteStatus
writeOnce var a = do
  b <- tryPutMVar (untouchable var) a
  pure $ if b then WriteSuccess else AlreadyWritten

{-# NOINLINE dbTestConfigVar #-}
dbTestConfigVar :: MVar TestDBInitConfig
dbTestConfigVar = unsafePerformIO newEmptyMVar

readOnlyTestDBConfig :: ReadOnly TestDBInitConfig
readOnlyTestDBConfig = ReadOnly dbTestConfigVar

data TestDBInitConfig = TestDBInitConfig
  { testDBInitConfigTemplateName :: String,
    testDBInitConfigHost :: String,
    testDBInitConfigPort :: String,
    testDBInitConfigUser :: String
  }

data TestDBConfig = TestDBConfig
  { testDBConfigTemplateName :: String,
    testDBConfigHost :: String,
    testDBConfigPort :: String,
    testDBConfigUser :: String,
    testDBConfigDBname :: String
  }

mkPerTestConfig :: String -> TestDBInitConfig -> TestDBConfig
mkPerTestConfig dbName TestDBInitConfig {..} =
  TestDBConfig
    { testDBConfigTemplateName = testDBInitConfigTemplateName,
      testDBConfigHost = testDBInitConfigHost,
      testDBConfigPort = testDBInitConfigPort,
      testDBConfigUser = testDBInitConfigUser,
      testDBConfigDBname = dbName
    }

toSettingString :: TestDBConfig -> Settings
toSettingString TestDBConfig {..} =
  fold
    [ "host=",
      fmt testDBConfigHost,
      " ",
      "port=",
      fmt testDBConfigPort,
      " ",
      "dbname=",
      fmt testDBConfigDBname,
      " ",
      "user=",
      fmt testDBConfigUser
    ]
  where
    fmt :: String -> ByteString
    fmt = toStrict . encodeUtf8 . TL.pack

--------------------------------------------------------------------------------

newtype TestDB a = TestDB {runTestDB :: (Connection, OTEL.Tracer) -> Log.LoggerEnv -> IO a}
  deriving
    (Functor, Applicative, Monad, MonadReader (Connection, OTEL.Tracer), MonadIO, MonadThrow, MonadCatch, MonadUnliftIO, Log.MonadLog)
    via ReaderT (Connection, OTEL.Tracer) (Log.LogT IO)

instance MonadDB TestDB where
  runDB :: Session a -> TestDB (Either UsageError a)
  runDB session = do
    conn <- asks Has.getter
    first SessionUsageError <$> liftIO (run session conn)

newtype TestSetupException = TestSetupException Text deriving (Show)

instance Exception TestSetupException

instance Example (TestDB a) where
  type Arg (TestDB a) = TestDBConfig

  evaluateExample ::
    TestDB a -> Params -> (ActionWith (Arg (TestDB a)) -> IO ()) -> ProgressCallback -> IO Result
  evaluateExample example params actionWith =
    evaluateExample
      (actionWith $ void . flip bracketConn example)
      params
      (\f -> f ())

--------------------------------------------------------------------------------

-- No-Op Trace Exporter for test suite
noOpExporter :: SpanExporter
noOpExporter = SpanExporter (\_ -> pure Success) (pure ())

withTracer :: (OTEL.TracerProvider -> (OTEL.TracerOptions -> OTEL.Tracer) -> IO c) -> IO c
withTracer f =
  let acquire' = do
        providerOpts <- snd <$> OTEL.getTracerProviderInitializationOptions
        processor <- simpleProcessor . SimpleProcessorConfig $ noOpExporter
        OTEL.createTracerProvider [processor] providerOpts
      release' _ = pure ()
      work tracerProvider = f tracerProvider $ OTEL.makeTracer tracerProvider "test-suite"
   in bracket acquire' release' work

--------------------------------------------------------------------------------

-- No-Op Logger for test suite
logger :: Log.Logger
logger = Log.Internal.Logger (\_ -> pure ()) (pure ()) (pure ())

loggerEnv :: Log.LoggerEnv
loggerEnv = Log.LoggerEnv logger "test-suite" [] [] Log.defaultLogLevel

--------------------------------------------------------------------------------

bracketConn :: TestDBConfig -> TestDB a -> IO a
bracketConn perTestConfig actionTestDB =
  withTracer $ \_tracerProvider mkTracer -> do
    let tracer = mkTracer OTEL.tracerOptions
    bracket
      ( do
          connection <- acquire $ toSettingString perTestConfig
          case connection of
            Left e -> throwIO (TestSetupException $ "Failed to connect to test database: \n" <> T.pack (show e))
            Right conn -> pure conn
      )
      release
      (\connection -> runTestDB actionTestDB (connection, tracer) loggerEnv)

withAuth :: SpecWith (Authz, TestDBConfig) -> SpecWith TestDBConfig
withAuth = beforeWith getAuth
  where
    getAuth :: TestDBConfig -> IO (Authz, TestDBConfig)
    getAuth cfg =
      bracketConn cfg $ do
        pass <- hashPassword $ mkPassword "foo"
        auth <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
          uid <- TRX.statement () $ User.insertUser $ User.ModelInsert (mkEmailAddress "user@host.com") pass (mkDisplayNameUnsafe "user") (mkFullNameUnsafe "fullUserName") Nothing True
          u <- TRX.statement () $ User.getUser $ getOneRow uid
          sm <- TRX.statement () $ Session.insertServerSession $ Session.ServerSessionInsert (getOneRow uid) Nothing Nothing (read "2099-01-01 10:30:20 UTC")
          pure $ Authz (User.toDomain $ fromMaybe (error "withAuth failure: Failed to look up user") u) (Session.toDomain $ getOneRow sm)
        case auth of
          Left err -> error $ "withAuth failure: " <> show err
          Right authz -> pure (authz, cfg)

withTestDB :: SpecWith TestDBConfig -> Spec
withTestDB = around $ bracket before after . during
  where
    before :: IO TestDBConfig
    before = do
      cfg@TestDBInitConfig {..} <- onlyRead readOnlyTestDBConfig
      uuid <- nextRandom
      let dbname = "servicehub-test-db-" <> toString uuid
      -- TODO: see if using `createdb` instead of `psql` is faster
      (ec, _, stde) <-
        readProcess $
          proc
            "psql"
            [ "-U",
              testDBInitConfigUser,
              "-d",
              "postgres",
              "-h",
              testDBInitConfigHost,
              "-p",
              testDBInitConfigPort,
              "-c",
              "CREATE DATABASE \"" <> dbname <> "\" TEMPLATE \"" <> testDBInitConfigTemplateName <> "\""
            ]
      when (ec /= ExitSuccess) $ do
        print stde
        throw (TestSetupException $ "Failed to create test database: \n" <> decodeUtf8 (toStrict stde))

      pure $ mkPerTestConfig dbname cfg

    after :: TestDBConfig -> IO ()
    after TestDBConfig {..} = do
      (ec, _, stde) <-
        readProcess $
          proc
            "psql"
            [ "-U",
              testDBConfigUser,
              "-d",
              "postgres",
              "-h",
              testDBConfigHost,
              "-p",
              testDBConfigPort,
              "-c",
              "DROP DATABASE \"" <> testDBConfigDBname <> "\" WITH (FORCE)"
            ]

      when (ec /= ExitSuccess) $
        throw (TestSetupException $ "Failed to create test database: \n" <> decodeUtf8 (toStrict stde))

    during :: ActionWith TestDBConfig -> TestDBConfig -> IO ()
    during = ($)

dbit :: String -> TestDB a -> SpecWith TestDBConfig
dbit = it
