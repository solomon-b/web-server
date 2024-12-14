module App.Monad where

--------------------------------------------------------------------------------

import App.Config
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
import Data.Text qualified as Text
import Data.Time (getCurrentTime)
import Effects.Clock (MonadClock (..))
import Effects.Database.Class
import Effects.MailSender (MonadEmail (..))
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Session qualified as HSQL
import Log qualified
import Network.Mail.Mime qualified as Mime
import Network.Mail.SMTP qualified as SMTP
import OpenTelemetry.Trace qualified as OTEL
import OpenTelemetry.Trace.Monad (MonadTracer (..))

--------------------------------------------------------------------------------

newtype AppM ctx a = AppM {runAppM :: AppContext ctx -> IO a}
  deriving
    (Functor, Applicative, Monad, MonadReader (AppContext ctx), MonadIO, MonadThrow, MonadCatch, MonadUnliftIO)
    via ReaderT (AppContext ctx) IO

instance MonadClock (AppM ctx) where
  currentSystemTime = liftIO getCurrentTime

instance MonadDB (AppM ctx) where
  runDB :: HSQL.Session a -> AppM ctx (Either HSQL.Pool.UsageError a)
  runDB s = do
    pool <- Reader.asks Has.getter
    liftIO $ HSQL.Pool.use pool s

instance MonadEmail (AppM ctx) where
  sendEmail :: Mime.Mail -> AppM ctx ()
  sendEmail mail = do
    Reader.asks Has.getter >>= \case
      Nothing -> do
        pure ()
      Just SmtpConfig {..} ->
        liftIO $ SMTP.sendMailWithLoginTLS (Text.unpack smtpConfigServer) (Text.unpack smtpConfigUsername) (Text.unpack smtpConfigPassword) mail

instance MonadTracer (AppM ctx) where
  getTracer :: AppM ctx OTEL.Tracer
  getTracer = Reader.asks Has.getter

instance Log.MonadLog (AppM ctx) where
  logMessage :: Log.LogLevel -> Text -> Aeson.Value -> AppM ctx ()
  logMessage level message json = AppM $ \AppContext {appLoggerEnv} -> do
    time <- getCurrentTime
    Log.logMessageIO appLoggerEnv time level message json

  localData :: [Aeson.Pair] -> AppM ctx a -> AppM ctx a
  localData data_ =
    AppM . Reader.local (\ctx@AppContext {appLoggerEnv = e} -> ctx {appLoggerEnv = e {Log.leData = data_ ++ Log.leData e}}) . runAppM

  localDomain :: Text -> AppM ctx a -> AppM ctx a
  localDomain domain =
    AppM . Reader.local (\ctx@AppContext {appLoggerEnv = e} -> ctx {appLoggerEnv = e {Log.leDomain = Log.leDomain e ++ [domain]}}) . runAppM

  localMaxLogLevel :: Log.LogLevel -> AppM ctx a -> AppM ctx a
  localMaxLogLevel level =
    AppM . Reader.local (\ctx@AppContext {appLoggerEnv = e} -> ctx {appLoggerEnv = e {Log.leMaxLogLevel = level}}) . runAppM

  getLoggerEnv :: AppM ctx Log.LoggerEnv
  getLoggerEnv = AppM $ \AppContext {appLoggerEnv} -> pure appLoggerEnv
