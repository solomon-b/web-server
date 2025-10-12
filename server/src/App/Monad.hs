{-# LANGUAGE StandaloneDeriving #-}
module App.Monad where

--------------------------------------------------------------------------------

import App.Context
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Control.Monad.Reader qualified as Reader
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Has qualified as Has
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Effects.Clock (MonadClock (..))
import Effects.Database.Class
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import OpenTelemetry.Trace.Monad (MonadTracer (..))

--------------------------------------------------------------------------------

newtype AppM ctx a = AppM {runAppM :: AppContext ctx -> IO a}
  deriving
    (Functor, Applicative, Monad, MonadReader (AppContext ctx), MonadIO, MonadThrow, MonadCatch, MonadUnliftIO)
    via ReaderT (AppContext ctx) IO

instance MonadClock (AppM ctx) where
  currentSystemTime = liftIO getCurrentTime

deriving via (ReaderT (AppContext ctx) IO) instance MonadDB (AppM ctx)

instance MonadTracer (AppM ctx) where
  getTracer :: AppM ctx OTEL.Tracer
  getTracer = Reader.asks Has.getter

instance Log.MonadLog (AppM ctx) where
  logMessage :: Log.LogLevel -> Text -> Aeson.Value -> AppM ctx ()
  logMessage level message json = do
    appLoggerEnv <- Reader.asks Has.getter
    time <- currentSystemTime
    liftIO $ Log.logMessageIO appLoggerEnv time level message json

  localData :: [Aeson.Pair] -> AppM ctx a -> AppM ctx a
  localData data_ =
    Reader.local $ Has.modifier $ \e -> e {Log.leData = data_ ++ Log.leData e}

  localDomain :: Text -> AppM ctx a -> AppM ctx a
  localDomain domain =
    Reader.local $ Has.modifier $ \e -> e {Log.leDomain = Log.leDomain e ++ [domain]}

  localMaxLogLevel :: Log.LogLevel -> AppM ctx a -> AppM ctx a
  localMaxLogLevel level =
    Reader.local $ Has.modifier $ \e -> e {Log.leMaxLogLevel = level}

  getLoggerEnv :: AppM ctx Log.LoggerEnv
  getLoggerEnv = Reader.asks Has.getter
