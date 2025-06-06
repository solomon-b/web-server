module API.Store.Id.Get where

--------------------------------------------------------------------------------

import API.Markdown.Post qualified as Markdown
import App.Auth qualified as Auth
import App.Errors (NotFound (..), throwErr)
import Component.Frame (loadFrameWithNav)
import Control.Lens (set)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Data.Text.Display (display)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.Images qualified as Images
import Effects.Database.Tables.Products (Domain (..))
import Effects.Database.Tables.Products qualified as Products
import Effects.Observability qualified as Observability
import Log qualified
import Lucid (class_, div_, h1_, id_)
import Lucid qualified
import OpenTelemetry.Trace qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Text.XmlHtml qualified as Xml
import Text.XmlHtml.Optics

--------------------------------------------------------------------------------

type Route =
  Servant.Header "Cookie" Text
    :> Servant.Header "HX-Request" Bool
    :> "store"
    :> Servant.Capture "id" Products.Id
    :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "Vary" Text] (Lucid.Html ()))

--------------------------------------------------------------------------------

template :: Lucid.Html () -> Lucid.Html ()
template content =
  div_ [class_ "flex flex-col justify-center items-center w-full"] $
    div_
      [id_ "content", class_ "p-4 my-8 flex flex-col flex-auto"]
      content

--------------------------------------------------------------------------------

renderImage :: Text -> Text
renderImage fp = mconcat ["<img src='", fp, "' />"]

renderProduct :: Products.Domain -> Lucid.Html () -> Lucid.Html ()
renderProduct (Products.Domain {dName, dHeroImage}) postContent =
  let heroImage :: Text
      heroImage = maybe "" (renderImage . Images.dFilePath) dHeroImage
   in do
        h1_ [class_ "mb-4 text-4xl font-extrabold leading-none tracking-tight text-gray-900 text-6xl"] (Lucid.toHtml dName)
        Lucid.toHtmlRaw heroImage
        div_
          [id_ "contentBody"]
          postContent

--------------------------------------------------------------------------------

handler ::
  forall m env.
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    MonadReader env m,
    Has Trace.Tracer env,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  Maybe Text ->
  Maybe Bool ->
  Products.Id ->
  m (Servant.Headers '[Servant.Header "Vary" Text] (Lucid.Html ()))
handler cookie hxTrigger bid =
  Observability.handlerSpan "GET /store/:id" bid (display . Servant.getResponse) $ do
    loginState <- Auth.userLoginState cookie
    post <- maybe (throwErr NotFound) (pure . Products.toDomain) =<< execQuerySpanThrow (Products.get bid)
    bodyNodes <- Markdown.processInput (dDescription post)
    let postFragment = renderProduct post bodyNodes
    let page = template postFragment

    case hxTrigger of
      Just True -> do
        pure $ Servant.addHeader "HX-Request" page
      _ -> do
        pageWithFrame <- loadFrameWithNav loginState "store-tab" page
        pure $ Servant.addHeader "HX-Request" pageWithFrame

swapTableFragment :: [Xml.Node] -> [Xml.Node] -> [Xml.Node]
swapTableFragment x = fmap (set (_id "content" . _elChildren) x)

swapContentBody :: [Xml.Node] -> [Xml.Node] -> [Xml.Node]
swapContentBody x = fmap (set (_id "contentBody" . _elChildren) x)
