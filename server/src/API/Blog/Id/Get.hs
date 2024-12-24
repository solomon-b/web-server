{-# LANGUAGE QuasiQuotes #-}

module API.Blog.Id.Get where

--------------------------------------------------------------------------------

import API.Markdown.Post qualified as Markdown
import App.Auth qualified as Auth
import App.Errors (NotFound (..), throwErr)
import Component.Frame (loadFrameWithNav)
import Control.Lens (set, (<&>))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.ByteString (ByteString)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Text.Encoding qualified as TE
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.BlogPosts (Domain (dContent))
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.Images qualified as Images
import Effects.Observability qualified as Observability
import Log qualified
import OpenTelemetry.Trace qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML, RawHtml (..), parseFragment, renderDocument, renderNodes)
import Text.XmlHtml qualified as Xml
import Text.XmlHtml.Optics

--------------------------------------------------------------------------------

type Route = Servant.Header "Cookie" Text :> Servant.Header "HX-Request" Bool :> "blog" :> Servant.Capture "id" BlogPosts.Id :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "Vary" Text] RawHtml)

--------------------------------------------------------------------------------

template :: ByteString
template =
  [i|<div class='flex flex-col justify-center items-center w-full'>
  <div id='content' class='p-4 my-8 flex flex-col flex-auto'></div>
</div>
|]

--------------------------------------------------------------------------------
-- Components

renderImage :: Text -> Text
renderImage fp = mconcat ["<img src='", fp, "' />"]

renderBlogPost :: BlogPosts.Domain -> Text
renderBlogPost (BlogPosts.Domain {dTitle, dHeroImage}) =
  let heroImage :: Text
      heroImage = maybe "" (renderImage . Images.dFilePath) dHeroImage
   in [i|
<h1 class="mb-4 text-4xl font-extrabold leading-none tracking-tight text-gray-900 text-6xl">#{BlogPosts.getSubject dTitle}</h1>
  #{heroImage}
  <div id="contentBody">
  </div>
|]

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
  BlogPosts.Id ->
  m (Servant.Headers '[Servant.Header "Vary" Text] RawHtml)
handler cookie hxTrigger bid =
  Observability.handlerSpan "GET /blog/:id" bid (display . Servant.getResponse) $ do
    loginState <- Auth.userLoginState cookie
    post <- maybe (throwErr NotFound) (pure . BlogPosts.toDomain) =<< execQuerySpanThrow (BlogPosts.getBlogPost bid)
    bodyNodes <- Markdown.processInput (BlogPosts.getBody $ dContent post)
    postFragment <- parseFragment $ TE.encodeUtf8 $ renderBlogPost post
    pageFragment <- parseFragment template <&> swapContentBody bodyNodes . swapTableFragment postFragment

    case hxTrigger of
      Just True -> do
        let html = renderNodes pageFragment
        pure $ Servant.addHeader "HX-Request" html
      _ -> do
        page <- loadFrameWithNav loginState "blog-tab" pageFragment
        let html = renderDocument page
        pure $ Servant.addHeader "HX-Request" html

swapTableFragment :: [Xml.Node] -> [Xml.Node] -> [Xml.Node]
swapTableFragment x = fmap (set (_id "content" . _elChildren) x)

swapContentBody :: [Xml.Node] -> [Xml.Node] -> [Xml.Node]
swapContentBody x = fmap (set (_id "contentBody" . _elChildren) x)
