{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module API.Blog.Get where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import Component.Frame (loadFrameWithNav)
import Control.Lens (over, (<&>))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Text.Encoding qualified as TE
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Observability qualified as Observability
import Log qualified
import OpenTelemetry.Trace.Core qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML, RawHtml, parseFragment, renderDocument)
import Text.XmlHtml qualified as Xml
import Text.XmlHtml.Optics (_elChildren, _id)

--------------------------------------------------------------------------------

type Route = Servant.Header "Cookie" Text :> "blog" :> Servant.Get '[HTML] RawHtml

--------------------------------------------------------------------------------
-- Components

loginButton :: ByteString
loginButton =
  [i|<button hx-get="/blog/new" hx-target='\#content' hx-push-url='true' class="w-32 text-white bg-green-700 hover:bg-green-800 focus:ring-4 focus:outline-none focus:ring-green-300 font-medium rounded-lg text-sm px-5 py-2.5 text-center">
    Create
</button>
|]

template :: Auth.LoggedIn -> ByteString
template loggedIn =
  let button = bool "" loginButton (Auth.isLoggedIn loggedIn)
   in [i|<div class='flex flex-col justify-center items-center w-full'>
  <div id='content' class='p-4 my-8 flex flex-wrap'>
  </div>
  <div class='flex w-full'>
    <span class='flex-auto'></span>
    #{button}
  </div>
</div>
|]

renderImage :: Maybe Text -> Text
renderImage = maybe "" (\fp -> mconcat ["<img src='", fp, "' />"])

blogPost :: BlogPosts.Domain -> Text
blogPost (BlogPosts.Domain {dId, dTitle, dHeroImagePath}) =
  [i|<div hx-get='/blog/#{display dId}' hx-target='\#content' hx-push-url="true" class='m-4 w-64 h-64'>
       #{renderImage dHeroImagePath}
       <p class='font-medium text-gray-500'>#{display dTitle}</p>
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
  m RawHtml
handler cookie =
  Observability.handlerSpan "GET /blog" () display $ do
    loginState <- Auth.userLoginState cookie
    posts <- fmap BlogPosts.toDomain <$> execQuerySpanThrow BlogPosts.getBlogPosts
    postsFragment <- parseFragment $ TE.encodeUtf8 $ foldMap blogPost posts

    pageFragment <- parseFragment (template loginState) <&> swapTableFragment (postsFragment <>)
    page <- loadFrameWithNav loginState "blog-tab" pageFragment

    pure $ renderDocument page

swapTableFragment :: ([Xml.Node] -> [Xml.Node]) -> [Xml.Node] -> [Xml.Node]
swapTableFragment x = fmap (over (_id "content" . _elChildren) x)
