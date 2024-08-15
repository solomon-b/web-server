module API.User.Login.Get where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Effects.Observability qualified as Observability
import OpenTelemetry.Trace qualified as Trace
import OrphanInstances.Lucid ()
import Servant ((:>))
import Servant qualified
import Utils.HTML (HTML, RawHtml, toHTML)
import Widgets.LoginForm qualified as LoginForm

--------------------------------------------------------------------------------

type Route = "user" :> "login" :> Servant.Get '[HTML] RawHtml

--------------------------------------------------------------------------------

handler ::
  ( Applicative m,
    Has Trace.Tracer env,
    MonadCatch m,
    MonadUnliftIO m,
    MonadReader env m
  ) =>
  m RawHtml
handler =
  Observability.handlerSpan "GET /user/login" () (const @Text "RawHtml") $ do
    pure $ toHTML LoginForm.widget
