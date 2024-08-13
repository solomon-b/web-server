module App.Monad where

--------------------------------------------------------------------------------

import App.Context
import Config
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Control.Monad.Reader qualified as Reader
import Data.Has qualified as Has
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

newtype AppM ctx a = AppM {runAppM' :: AppContext ctx -> Log.LoggerEnv -> IO a}
  deriving
    (Functor, Applicative, Monad, MonadReader (AppContext ctx), MonadIO, MonadThrow, MonadCatch, MonadUnliftIO, Log.MonadLog)
    via ReaderT (AppContext ctx) (Log.LogT IO)

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
    SmtpConfig {..} <- Reader.asks Has.getter
    liftIO $ SMTP.sendMailWithLoginTLS (Text.unpack smtpConfigServer) (Text.unpack smtpConfigUsername) (Text.unpack smtpConfigPassword) mail

instance MonadTracer (AppM ctx) where
  getTracer :: AppM ctx OTEL.Tracer
  getTracer = Reader.asks Has.getter
