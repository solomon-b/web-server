module API.User.Login.Get where

--------------------------------------------------------------------------------

import Component.Forms.Login (template)
import Component.Frame (loadFrame)
import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.EmailAddress (EmailAddress)
import Effects.Observability qualified as Observability
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  "user"
    :> "login"
    :> Servant.Header "HX-Current-Url" Text
    :> Servant.QueryParam "redirect" Text
    :> Servant.QueryParam "email" EmailAddress
    :> Servant.Get '[HTML] (Lucid.Html ())

--------------------------------------------------------------------------------

handler ::
  ( Applicative m,
    Has Trace.Tracer env,
    MonadCatch m,
    Log.MonadLog m,
    MonadUnliftIO m,
    MonadReader env m
  ) =>
  Maybe Text ->
  Maybe Text ->
  Maybe EmailAddress ->
  m (Lucid.Html ())
handler hxCurrentUrl redirectQueryParam emailQueryParam =
  Observability.handlerSpan "GET /user/login" () display $ do
    let loginForm = template emailQueryParam $ hxCurrentUrl <|> redirectQueryParam
    loadFrame loginForm
