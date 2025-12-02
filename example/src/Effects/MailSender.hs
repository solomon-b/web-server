{-# LANGUAGE UndecidableInstances #-}

module Effects.MailSender where

--------------------------------------------------------------------------------

import App.Config
import App.Context () -- Import for orphan Has instance
import App.Monad (AppM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader qualified as Reader
import Data.Has qualified as Has
import Data.Text qualified as Text
import Network.Mail.Mime qualified as Mime
import Network.Mail.SMTP qualified as SMTP

--------------------------------------------------------------------------------

class MonadEmail m where
  sendEmail :: Mime.Mail -> m ()

instance (Has.Has (Maybe SmtpConfig) ctx) => MonadEmail (AppM ctx) where
  sendEmail :: Mime.Mail -> AppM ctx ()
  sendEmail mail = do
    Reader.asks Has.getter >>= \case
      Nothing -> do
        pure ()
      Just SmtpConfig {..} ->
        liftIO $ SMTP.sendMailWithLoginTLS (Text.unpack smtpConfigServer) (Text.unpack smtpConfigUsername) (Text.unpack smtpConfigPassword) mail
