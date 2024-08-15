module API.User.Register.Get where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Effects.Observability qualified as Observability
import Htmx.Lucid.Head qualified as Lucid.Htmx
import Lucid
import OpenTelemetry.Trace qualified as Trace
import OrphanInstances.Lucid ()
import Servant ((:>))
import Servant qualified
import Utils.HTML (HTML, RawHtml, classes_, toHTML)
import Widgets.RegisterForm qualified as RegisterForm

--------------------------------------------------------------------------------

type Route = "user" :> "register" :> Servant.Get '[HTML] RawHtml

--------------------------------------------------------------------------------

data Page = Page

header :: (Monad m) => HtmlT m ()
header =
  head_ $ do
    title_ "web-server"
    script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Html ())
    Lucid.Htmx.useHtmx

page :: (Monad m) => Lucid.HtmlT m ()
page =
  Lucid.doctypehtml_ $ do
    header

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
