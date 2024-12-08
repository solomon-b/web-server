{-# LANGUAGE QuasiQuotes #-}

module API.Blog.Id.Edit.Get where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Errors (NotFound (..), Unauthorized (..), throwErr)
import Component.Frame (loadFrameWithNav)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Has (Has)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import Log qualified
import OpenTelemetry.Trace qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML, RawHtml, parseFragment, renderDocument, renderNodes)
import Text.XmlHtml qualified as Xml
import Text.XmlHtml.Optics

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> Servant.Header "HX-Request" Bool
    :> "blog"
    :> Servant.Capture "id" BlogPosts.Id
    :> "edit"
    :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "Vary" Text] RawHtml)

--------------------------------------------------------------------------------

contentField :: Text -> ByteString
contentField content =
  [i|
<div id='content-field'>
  <label for='content' class='mb-2 text-sm text-gray-900 font-semibold'>Add body</label>
  <div class='flex flex-col border border-gray-300 rounded-lg'>
      <div class='flex mb-2'>
          <div class='p-2 border-r border-gray-300 rounded-t-lg bg-white text-gray-900'>
            <button href='\#' role='tab' hx-get='/blog/new/edit' hx-swap='innerHTML' hx-target='\#content-field'>
              Write
            </button>
          </div>
  
          <div class='p-2 border-b border-gray-300 rounded-t-lg bg-gray-50 text-gray-500'>
            <button href='\#' role='tab' hx-get='/blog/new/preview' hx-swap='innerHTML' hx-target='\#content-field'>
              Preview
            </button>
          </div>
  
          <div class='p-2 border-b border-gray-300 rounded-t-lg bg-gray-50 text-gray-500 grow flex justify-end'>
              <i title='Heading' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-heading'></i>
              <i title='Bold' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-bold'></i>
              <i title='Italic' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-italic'></i>
              <i class='border-l border-t-0 border-gray-300'></i>
              <i title='Quote' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-quote-left'></i>
              <i title='Code' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-code'></i>
              <i title='Link' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-link'></i>
              <i class='border-l border-t-0 border-gray-300'></i>
              <i title='Numbered List' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-list-ol'></i>
              <i title='Unordered List' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-list'></i>
              <i title='Task List' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-list-check'></i>
          </div>
      </div>
      <div class='m-2 min-h-60'>
          <textarea name='content' placeholder='Add your content here...' rows='11' class='block p-2.5 w-full text-sm text-gray-900 bg-gray-50 rounded-lg border border-gray-300 focus:ring-blue-500 focus:border-blue-500'>#{content}</textarea>
      </div>
  </div>
</div>
|]

publishToggle :: Bool -> ByteString
publishToggle isPublished =
  let checked :: Text
      checked = bool "" "checked" isPublished
   in [i|
<div class='px-5 font-medium inline-flex'>
<label class='inline-flex items-center cursor-pointer'>
  <span class='px-2.5 ms-3 text-sm font-medium text-gray-900 font-semibold'>Published</span>
  <input type='checkbox' name='published' class='sr-only peer' value='true' #{checked} />
  <div class="relative w-11 h-6 bg-gray-200 peer-focus:outline-none peer-focus:ring-4 peer-focus:ring-green-300 dark:peer-focus:ring-green-800 rounded-full peer dark:bg-gray-700 peer-checked:after:translate-x-full rtl:peer-checked:after:-translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:start-[2px] after:bg-white after:border-gray-300 after:border after:rounded-full after:w-5 after:h-5 after:transition-all peer-checked:bg-green-600"></div>
</label>
</div>
|]

titleField :: Text -> ByteString
titleField title =
  [i|
<div>
  <label for='title' class='mb-2 text-sm text-gray-900 font-semibold'>Add title</label>
  <input type='text' placeholder='title' name='title' id='title' value='#{title}' class='bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-green-500 focus:border-green-500 block w-full p-2.5' />
</div>
|]

fileUploadField :: Maybe Text -> ByteString
fileUploadField _heroImagePath =
  [i|
<div>
  <label class="block mb-2 text-sm font-medium text-gray-900 font-semibold" for="hero_image_input">Upload Hero Image</label>
  <input type="file" name="heroImagePath" class="block w-full text-sm text-gray-900 border border-gray-300 rounded-lg cursor-pointer bg-gray-50 focus:outline-none" id="hero_image_input">
</div>
|]

submitButton :: Bool -> ByteString
submitButton isPublished =
  [i|
<div class='flex justify-end'>
  #{publishToggle isPublished}
  <button id="publishedStatusButton" data-dropdown-toggle="publishedDropdown" type='submit' class='text-white bg-green-700 hover:bg-green-800 focus:ring-4 focus:outline-none focus:ring-green-300 font-medium rounded-lg text-sm px-5 py-2.5 text-center inline-flex items-center'>
    Update Post
  </button>
</div>
|]

template :: BlogPosts.Id -> Text -> Text -> Bool -> Maybe Text -> ByteString
template bid title content isPublished heroImagePath =
  [i|
<div class='relative p-4 w-full max-w-4xl max-h-full mx-auto'>
  <div class='flex items-center justify-between p-4 md:p-5'>
      <h3 class='text-xl font-semibold text-gray-900'>Create Post</h3>
  </div>
  <div class='p-4 md:p-5'>
      <form hx-post='/blog/#{bid}/edit' class='space-y-4 flex flex-col' data-bitwarden-watching='1' enctype="multipart/form-data">
          #{titleField title}
          #{fileUploadField heroImagePath}
          #{contentField content}
          #{submitButton isPublished}
      </form>
  </div>
</div>
|]

--------------------------------------------------------------------------------

handler ::
  ( Applicative m,
    Has Trace.Tracer env,
    Log.MonadLog m,
    MonadCatch m,
    MonadDB m,
    MonadUnliftIO m,
    MonadReader env m
  ) =>
  Auth.Authz ->
  Maybe Bool ->
  BlogPosts.Id ->
  m (Servant.Headers '[Servant.Header "Vary" Text] RawHtml)
handler (Auth.Authz user@User.Domain {dId = uid, ..} _) hxTrigger bid =
  Observability.handlerSpan "GET /post/new" () (display . Servant.getResponse) $ do
    BlogPosts.Domain {..} <- maybe (throwErr NotFound) (pure . BlogPosts.toDomain) =<< execQuerySpanThrow (BlogPosts.getBlogPost bid)
    unless (dIsAdmin || uid == dAuthorId) $ throwErr Unauthorized

    pageFragment <- parseFragment $ template bid dTitle dContent dPublished dHeroImagePath
    page <- loadFrameWithNav (Auth.IsLoggedIn user) "blog-tab" pageFragment

    case hxTrigger of
      Just True ->
        pure $ Servant.addHeader "HX-Request" $ renderNodes pageFragment
      _ -> do
        let html = renderDocument $ swapMain pageFragment page
        pure $ Servant.addHeader "HX-Request" html

--------------------------------------------------------------------------------

swapMain :: [Xml.Node] -> Xml.Document -> Xml.Document
swapMain = swapInner _main
