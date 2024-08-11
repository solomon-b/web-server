module API.MailingList.Post where

--------------------------------------------------------------------------------

import Config (Hostname (..), SmtpConfig (..))
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader qualified as Reader
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.CaseInsensitive qualified as CI
import Data.Has (Has)
import Data.Has qualified as Has
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display)
import Data.Text.Encoding qualified as Text.Encoding
import Domain.Types.EmailAddress (EmailAddress (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.MailingList qualified as MailingList
import Effects.MailSender
import Effects.Observability qualified as Observability
import Errors (Unauthorized (..), throwErr)
import GHC.Generics (Generic)
import Log qualified
import Lucid qualified
import Network.Mail.Mime qualified as Mime
import Network.Mail.SMTP qualified as SMTP
import OpenTelemetry.Trace qualified as OTEL
import OrphanInstances.OneRow ()
import Servant ((:>))
import Servant qualified
import Servant.HTML.Lucid qualified as Lucid
import Text.Email.Validate qualified as Email
import Web.FormUrlEncoded (FromForm)

--------------------------------------------------------------------------------
-- Route

newtype MailingListForm = MailingListForm
  {emailAddress :: EmailAddress}
  deriving stock (Show, Generic)
  deriving newtype (Display)
  deriving anyclass (FromJSON, ToJSON, FromForm)

type Route = "mailing-list" :> "signup" :> Servant.ReqBody '[Servant.JSON, Servant.FormUrlEncoded] MailingListForm :> Servant.Post '[Lucid.HTML] (Lucid.Html ())

--------------------------------------------------------------------------------
-- Handler

handler ::
  ( Has OTEL.Tracer env,
    Has SmtpConfig env,
    Has Hostname env,
    Log.MonadLog m,
    MonadCatch m,
    MonadDB m,
    MonadEmail m,
    MonadReader env m,
    MonadThrow m,
    MonadUnliftIO m
  ) =>
  MailingListForm ->
  m (Lucid.Html ())
handler req@(MailingListForm e@(EmailAddress {..})) = do
  Observability.handlerSpan "POST /mailing-list" req (const ()) $ do
    unless (Email.isValid $ Text.Encoding.encodeUtf8 $ CI.original emailAddress) $ throwErr Unauthorized

    _pid <- execQuerySpanThrow $ MailingList.insertEmailAddress $ MailingList.ModelInsert e
    Log.logInfo "Submited Email Address:" (KeyMap.singleton "email" (Text.unpack $ CI.original emailAddress))

    -- TODO: Disable email confirmation in Dev mode
    -- sendConfirmationEmail e

    pure $ Lucid.p_ "You have been added to the mailing list!"

sendConfirmationEmail ::
  ( MonadEmail m,
    MonadReader env m,
    Has Hostname env
  ) =>
  EmailAddress ->
  m ()
sendConfirmationEmail EmailAddress {..} = do
  Hostname hostname <- Reader.asks Has.getter
  let subject :: Text
      subject = "Welcome to " <> hostname

      body :: Mime.Part
      body = Mime.plainPart "Thank you for your interest. We are still in early development but will reach out soon with more information."

      mail :: Mime.Mail
      mail = SMTP.simpleMail (SMTP.Address Nothing $ "contact@" <> hostname) [SMTP.Address Nothing $ CI.original emailAddress] [] [] subject [body]
   in sendEmail mail