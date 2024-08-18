module API.User.Register.Get where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Effects.Observability qualified as Observability
import Lucid
import OpenTelemetry.Trace qualified as Trace
import OrphanInstances.Lucid ()
import Servant ((:>))
import Servant qualified
import Utils.HTML (HTML, RawHtml, classes_, toHTML)
import Widgets.Header qualified as Header
import Widgets.RegisterForm qualified as RegisterForm

--------------------------------------------------------------------------------

type Route = "user" :> "register" :> Servant.Get '[HTML] RawHtml

--------------------------------------------------------------------------------

data Page = Page

page :: (Monad m) => Lucid.HtmlT m ()
page =
  Lucid.doctypehtml_ $ do
    Header.widget

    Lucid.body_ $
      div_ [classes_ ["container", "mx-auto"]] $ do
        RegisterForm.widget

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
    pure $ toHTML page
