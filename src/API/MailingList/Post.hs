module API.MailingList.Post where

--------------------------------------------------------------------------------

import Config (Hostname (..), SmtpConfig (..))
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.CaseInsensitive qualified as CI
import Data.Has (Has)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display)
import Data.Text.Encoding qualified as Text.Encoding
import Domain.Types.Email (EmailAddress (..))
import Effects.Database.Class (MonadDB)
import Effects.Database.Queries.MailingList qualified as MailingList
import Effects.Email.Class
import Errors (throw401)
import GHC.Generics (Generic)
import Log qualified
import Lucid qualified
import Network.Mail.Mime qualified as Mime
import Network.Mail.SMTP qualified as SMTP
import OpenTelemetry.Trace qualified as OTEL
import OrphanInstances ()
import Servant ((:>))
import Servant qualified
import Servant.HTML.Lucid qualified as Lucid
import Text.Email.Validate qualified as Email
import Tracing qualified
import Web.FormUrlEncoded (FromForm)

--------------------------------------------------------------------------------
-- Route

newtype MailingListForm = MailingListForm
  {emailAddress :: EmailAddress}
  deriving stock (Show, Generic)
  deriving newtype (Display)
  deriving anyclass (FromJSON, ToJSON, FromForm)

type MailingListAPI =
  "signup" :> Servant.ReqBody '[Servant.JSON, Servant.FormUrlEncoded] MailingListForm :> Servant.Post '[Lucid.HTML] (Lucid.Html ())

--------------------------------------------------------------------------------
-- Handler

handler ::
  ( Log.MonadLog m,
    MonadReader env m,
    Has OTEL.Tracer env,
    Has SmtpConfig env,
    Has Hostname env,
    MonadDB m,
    MonadEmail m,
    MonadThrow m,
    MonadCatch m,
    MonadUnliftIO m
  ) =>
  MailingListForm ->
  m (Lucid.Html ())
handler req@(MailingListForm e@(EmailAddress {..})) = do
  Tracing.handlerSpan "/mailing-list" req (const ()) $ do
    unless (Email.isValid $ Text.Encoding.encodeUtf8 $ CI.original emailAddress) $ throw401 "Invalid Email Address"

    _pid <- MailingList.insertEmailAddress e
    Log.logInfo "Submited Email Address:" (KeyMap.singleton "email" (Text.unpack $ CI.original emailAddress))

    -- sendConfirmationEmail e

    pure $ Lucid.p_ "You have been added to the mailing list!"

sendConfirmationEmail :: (MonadEmail m) => EmailAddress -> m ()
sendConfirmationEmail EmailAddress {..} =
  let subject :: Text
      subject = "Welcome to kpbj.fm"

      body :: Mime.Part
      body = Mime.plainPart "Thank you for your interest. We are still in early development but will reach out soon with more information."

      mail :: Mime.Mail
      mail = SMTP.simpleMail (SMTP.Address Nothing "contact@example.com") [SMTP.Address Nothing $ CI.original emailAddress] [] [] subject [body]
   in sendEmail mail
