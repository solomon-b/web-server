{-# LANGUAGE QuasiQuotes #-}

module API.Store.Get where

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
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.Images qualified as Images
import Effects.Database.Tables.Products qualified as Products
import Effects.Observability qualified as Observability
import Log qualified
import Lucid (class_, div_, id_, p_)
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route = Servant.Header "Cookie" Text :> "store" :> Servant.Get '[HTML] (Lucid.Html ())

--------------------------------------------------------------------------------

renderImage :: Text -> Text
renderImage fp = mconcat ["<img src='", fp, "' />"]

productCard :: Products.Domain -> Lucid.Html ()
productCard (Products.Domain {dId, dName, dHeroImage}) =
  let heroImage :: Text
      heroImage = maybe "" (renderImage . Images.dFilePath) dHeroImage
   in div_ [hxGet_ [i|/store/#{display dId}|], hxTarget_ "#content", hxPushUrl_ "true", class_ "m-4 w-64 h-64"] $ do
        Lucid.toHtmlRaw heroImage
        p_ [class_ "font-medium text-gray-500"] (Lucid.toHtml $ display dName)

template :: [Products.Domain] -> Lucid.Html ()
template posts =
  div_ [class_ "flex flex-col justify-center items-center w-full"] $
    div_ [id_ "content", class_ "p-4 my-8 flex flex-wrap"] $
      div_ [class_ "flex w-full"] $
        Lucid.toHtml $
          foldMap productCard posts

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
  m (Lucid.Html ())
handler cookie =
  Observability.handlerSpan "GET /store" $ do
    loginState <- Auth.userLoginState cookie

    products <- fmap Products.toDomain <$> execQuerySpanThrow Products.gets
    let pageFragment = template products
    loadFrameWithNav loginState "store-tab" pageFragment
