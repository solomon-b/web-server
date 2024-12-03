{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant fmap" #-}

module API.Blog.Id.Get where

--------------------------------------------------------------------------------

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
import Effects.Observability qualified as Observability
import Log qualified
import OpenTelemetry.Trace qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML, RawHtml (..), parseFragment, renderDocument)
import Text.XmlHtml qualified as Xml
import Text.XmlHtml.Optics

--------------------------------------------------------------------------------

type Route = Servant.Header "Cookie" Text :> "blog" :> Servant.Capture "id" BlogPosts.Id :> Servant.Get '[HTML] RawHtml

--------------------------------------------------------------------------------

template :: ByteString
template =
  [i|<div class='flex flex-col justify-center items-center w-full'>
  <div id='content' class='p-4 my-8 flex flex-col flex-auto'></div>
</div>
|]

--------------------------------------------------------------------------------
-- Components

renderImage :: Maybe Text -> Text
renderImage = maybe "" (\fp -> mconcat ["<img src='", fp, "' />"])

renderBlogPost :: BlogPosts.Domain -> Text
renderBlogPost (BlogPosts.Domain {dContent, dTitle, dHeroImagePath}) =
  [i|<h1>#{dTitle}</h1>
       #{renderImage dHeroImagePath}
       <p>#{dContent}</p>
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
  BlogPosts.Id ->
  m RawHtml
handler cookie bid =
  Observability.handlerSpan "GET /blog/:id" bid display $ do
    loginState <- Auth.userLoginState cookie
    post <- maybe (throwErr NotFound) pure . fmap BlogPosts.toDomain =<< execQuerySpanThrow (BlogPosts.getBlogPost bid)
    postFragment <- parseFragment $ TE.encodeUtf8 $ renderBlogPost post

    pageFragment <- parseFragment template <&> swapTableFragment postFragment
    page <- loadFrameWithNav loginState "blog-tab" pageFragment

    pure $ renderDocument page

swapTableFragment :: [Xml.Node] -> [Xml.Node] -> [Xml.Node]
swapTableFragment x = fmap (set (_id "content" . _elChildren) x)
