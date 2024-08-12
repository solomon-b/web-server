{-# LANGUAGE TypeFamilies #-}

module Test.Database.Monad
  ( ReadOnly,
    WriteStatus (..),
    writeOnce,
    readOnlyTestDBConfig,
    withTestDB,
    bracketConn,
    dbit,
    TestDBInitConfig (..),
    TestDBConfig (..),
    TestDB (..),
    TestSetupException (..),
  )
where

import Control.Concurrent (MVar, newEmptyMVar)
import Control.Concurrent.MVar (readMVar, tryPutMVar)
import Control.Exception (Exception, throw, throwIO)
import Control.Monad (void, when)
import Control.Monad.Catch (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (runReaderT))
import Data.Bifunctor (first)
import Data.ByteString (ByteString, toStrict)
import Data.Foldable
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Effects.Database.Class (MonadDB (..))
import GHC.IO (unsafePerformIO)
import GHC.IO.Exception (ExitCode (..))
import Hasql.Connection (Connection, Settings, acquire, release)
import Hasql.Pool (UsageError (..))
import Hasql.Session (Session, run)
import System.Process.Typed (proc, readProcess)
import Test.Hspec (Spec, SpecWith, it)
import Test.Hspec.Core.Hooks (around)
import Test.Hspec.Core.Spec (ActionWith, Example (..), Params, ProgressCallback, Result)

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

newtype TestDB a = TestDB {runTestDB :: ReaderT Connection IO a}
  deriving newtype (Functor)
  deriving newtype (Applicative)
  deriving newtype (Monad)
  deriving newtype (MonadReader Connection)
  deriving newtype (MonadIO)

-- deriving newtype MonadUnliftIO
-- deriving newtype MonadThrow
-- deriving newtype MonadCatch

instance MonadDB TestDB where
  runDB :: Session a -> TestDB (Either UsageError a)
  runDB session = do
    conn <- ask
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

bracketConn :: TestDBConfig -> TestDB a -> IO a
bracketConn perTestConfig actionTestDB =
  bracket
    ( do
        connection <- acquire $ toSettingString perTestConfig
        case connection of
          Left e -> throwIO (TestSetupException $ "Failed to connect to test database: \n" <> T.pack (show e))
          Right conn -> pure conn
    )
    release
    (runReaderT (runTestDB actionTestDB))

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
