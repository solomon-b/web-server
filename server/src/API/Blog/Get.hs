{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module API.Blog.Get where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import Component.Frame (loadFrameWithNav)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.ByteString (ByteString)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.Images qualified as Images
import Effects.Observability qualified as Observability
import Log qualified
import OpenTelemetry.Trace.Core qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML, RawHtml, parseFragment, renderDocument)

--------------------------------------------------------------------------------

type Route = Servant.Header "Cookie" Text :> "blog" :> Servant.Get '[HTML] RawHtml

--------------------------------------------------------------------------------
-- Components

renderImage :: Text -> Text
renderImage fp = mconcat ["<img src='", fp, "' />"]

blogPost :: BlogPosts.Domain -> Text
blogPost (BlogPosts.Domain {dId, dTitle, dHeroImage}) =
  let heroImagePath :: Text
      heroImagePath = maybe "" (renderImage . Images.dFilePath) dHeroImage
   in [i|
<div hx-get='/blog/#{display dId}' hx-target='\#content' hx-push-url="true" class='m-4 w-64 h-64'>
  #{heroImagePath}
  <p class='font-medium text-gray-500'>#{display dTitle}</p>
</div>
|]

template :: [BlogPosts.Domain] -> ByteString
template posts =
  [i|
<div class='flex flex-col justify-center items-center w-full'>
  <div id='content' class='p-4 my-8 flex flex-wrap'>
    <div class='flex w-full'>
      #{foldMap blogPost posts}
    </div>
  </div>
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
    pageFragment <- parseFragment (template posts)
    page <- loadFrameWithNav loginState "blog-tab" pageFragment

    pure $ renderDocument page
