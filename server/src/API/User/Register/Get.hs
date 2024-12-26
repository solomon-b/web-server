module API.User.Register.Get where

--------------------------------------------------------------------------------

import Component.Forms.Register (template)
import Component.Frame (loadFrame)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text.Display (display)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import Effects.Observability qualified as Observability
import Log qualified
import OpenTelemetry.Trace qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML, RawHtml, renderDocument)

--------------------------------------------------------------------------------

type Route =
  "user"
    :> "register"
    :> Servant.QueryParam "emailAddress" EmailAddress
    :> Servant.QueryParam "displayName" DisplayName
    :> Servant.QueryParam "fullName" FullName
    :> Servant.Get '[HTML] RawHtml

--------------------------------------------------------------------------------

handler ::
  ( Applicative m,
    Has Trace.Tracer env,
    MonadCatch m,
    Log.MonadLog m,
    MonadUnliftIO m,
    MonadReader env m
  ) =>
  Maybe EmailAddress ->
  Maybe DisplayName ->
  Maybe FullName ->
  m RawHtml
handler emailAddress displayName fullName =
  Observability.handlerSpan "GET /user/register" (emailAddress, displayName, fullName) display $ do
    let registerForm = template displayName fullName emailAddress Nothing
    page <- loadFrame registerForm

    let html = renderDocument page
    pure html
