module API.User.Register.Get where

--------------------------------------------------------------------------------

import Component.Forms.Register (template)
import Component.Frame (loadFrame)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import Effects.Observability qualified as Observability
import Log qualified
import OpenTelemetry.Trace qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML, RawHtml, parseFragment, renderDocument, renderNodes)
import Text.XmlHtml.Optics

--------------------------------------------------------------------------------

type Route =
  "user"
    :> "register"
    :> Servant.Header "HX-Request" Bool
    :> Servant.QueryParam "emailAddress" EmailAddress
    :> Servant.QueryParam "displayName" DisplayName
    :> Servant.QueryParam "fullName" FullName
    :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "Vary" Text] RawHtml)

--------------------------------------------------------------------------------

handler ::
  ( Applicative m,
    Has Trace.Tracer env,
    MonadCatch m,
    Log.MonadLog m,
    MonadUnliftIO m,
    MonadReader env m
  ) =>
  Maybe Bool ->
  Maybe EmailAddress ->
  Maybe DisplayName ->
  Maybe FullName ->
  m (Servant.Headers '[Servant.Header "Vary" Text] RawHtml)
handler hxTrigger emailAddress displayName fullName =
  Observability.handlerSpan "GET /user/register" (emailAddress, displayName, fullName) (display . Servant.getResponse) $ do
    pageFragment <- parseFragment $ template displayName fullName emailAddress Nothing
    page <- loadFrame pageFragment

    case hxTrigger of
      Just True ->
        pure $ Servant.addHeader "HX-Request" $ renderNodes pageFragment
      _ -> do
        let html = renderDocument $ swapInner _main pageFragment page
        pure $ Servant.addHeader "HX-Request" html
