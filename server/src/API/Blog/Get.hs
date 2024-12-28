{-# LANGUAGE QuasiQuotes #-}

module API.Blog.Get where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import Component.Frame (loadFrameWithNav)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
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
import Lucid (class_, div_, id_, p_)
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace.Core qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML, RawHtml, renderLucid)

--------------------------------------------------------------------------------

type Route = Servant.Header "Cookie" Text :> "blog" :> Servant.Get '[HTML] RawHtml

--------------------------------------------------------------------------------

renderImage :: Text -> Text
renderImage fp = mconcat ["<img src='", fp, "' />"]

blogPost :: BlogPosts.Domain -> Lucid.Html ()
blogPost (BlogPosts.Domain {dId, dTitle, dHeroImage}) =
  let heroImagePath :: Text
      heroImagePath = maybe "" (renderImage . Images.dFilePath) dHeroImage
   in div_ [hxGet_ [i|/blog/#{display dId}|], hxTarget_ "#content", hxPushUrl_ "true", class_ "m-4 w-64 h-64"] $ do
        Lucid.toHtml heroImagePath
        p_ [class_ "font-medium text-gray-500"] (Lucid.toHtml $ display dTitle)

template :: [BlogPosts.Domain] -> Lucid.Html ()
template posts =
  div_ [class_ "flex flex-col justify-center items-center w-full"] $
    div_ [id_ "content", class_ "p-4 my-8 flex flex-wrap"] $
      div_ [class_ "flex w-full"] $
        Lucid.toHtml $
          foldMap blogPost posts

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
    let pageFragment = template posts
    page <- loadFrameWithNav loginState "blog-tab" pageFragment

    pure $ renderLucid page
