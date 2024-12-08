module API.MailingList.Post where

--------------------------------------------------------------------------------

import App.Config (Hostname (..), SmtpConfig (..))
import App.Errors (Unauthorized (..), throwErr)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader qualified as Reader
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Has (Has)
import Data.Has qualified as Has
import Data.Text (Text)
import Data.Text.Display (Display, display)
import Domain.Types.EmailAddress (EmailAddress, isValid)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.MailingList qualified as MailingList
import Effects.MailSender
import Effects.Observability qualified as Observability
import GHC.Generics (Generic)
import Log qualified
import Network.Mail.Mime qualified as Mime
import Network.Mail.SMTP qualified as SMTP
import OpenTelemetry.Trace qualified as OTEL
import OrphanInstances.OneRow ()
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML, RawHtml, renderNodes)
import Text.XmlHtml qualified as Xml
import Web.FormUrlEncoded (FromForm)

--------------------------------------------------------------------------------
-- Route

newtype MailingListForm = MailingListForm
  {emailAddress :: EmailAddress}
  deriving stock (Show, Generic)
  deriving newtype (Display)
  deriving anyclass (FromJSON, ToJSON, FromForm)

type Route = "mailing-list" :> "signup" :> Servant.ReqBody '[Servant.JSON, Servant.FormUrlEncoded] MailingListForm :> Servant.Post '[HTML] RawHtml

--------------------------------------------------------------------------------
-- Handler

handler ::
  ( Has OTEL.Tracer env,
    Has (Maybe SmtpConfig) env,
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
  m RawHtml
handler req@(MailingListForm emailAddress) = do
  Observability.handlerSpan "POST /mailing-list" req display $ do
    unless (isValid emailAddress) $ throwErr Unauthorized

    _pid <- execQuerySpanThrow $ MailingList.insertEmailAddress $ MailingList.ModelInsert emailAddress
    Log.logInfo "Submited Email Address:" (KeyMap.singleton "email" (display emailAddress))

    -- TODO: Dev Mode SMTP Server?
    sendConfirmationEmail emailAddress

    pure $ renderNodes [Xml.TextNode "You have been added to the mailing list!"]

sendConfirmationEmail ::
  ( MonadEmail m,
    MonadReader env m,
    Has Hostname env
  ) =>
  EmailAddress ->
  m ()
sendConfirmationEmail emailAddress = do
  Hostname hostname <- Reader.asks Has.getter
  let subject :: Text
      subject = "Welcome to " <> hostname

      body :: Mime.Part
      body = Mime.plainPart "Thank you for your interest. We are still in early development but will reach out soon with more information."

      mail :: Mime.Mail
      mail = SMTP.simpleMail (SMTP.Address Nothing $ "contact@" <> hostname) [SMTP.Address Nothing $ display emailAddress] [] [] subject [body]
   in sendEmail mail
