{-# LANGUAGE QuasiQuotes #-}

module API.Blog.New.Edit.Get where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Errors (Unauthorized (..), throwErr)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.ByteString (ByteString)
import Data.Foldable (fold)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import OpenTelemetry.Trace qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML, RawHtml, parseFragment, renderNodes)

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> "blog"
    :> "new"
    :> "edit"
    :> Servant.QueryParam "content" Text
    :> Servant.Get '[HTML] RawHtml

--------------------------------------------------------------------------------

headingButton :: ByteString
headingButton =
  let onClick :: Text
      onClick = [i|x-on:click="contentModel += '\#\#\# '; $refs.contentRef.focus()"|]
   in [i|<i #{onClick} title='Heading' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-heading'></i>|]

boldButton :: ByteString
boldButton =
  let onClick :: Text
      onClick =
        [i|x-on:click="contentModel += '****';
$nextTick(() => {
  $refs.contentRef.focus();
  const pos = $refs.contentRef.value.length - 2;
  $refs.contentRef.setSelectionRange(pos, pos);
})"|]
   in [i|<i #{onClick} title='Bold' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-bold'></i>|]

italicButton :: ByteString
italicButton =
  let onClick :: Text
      onClick =
        [i|x-on:click="contentModel += '__';
$nextTick(() => {
  $refs.contentRef.focus();
  const pos = $refs.contentRef.value.length - 1;
  $refs.contentRef.setSelectionRange(pos, pos);
})"|]
   in [i|<i #{onClick} title='Italic' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-italic'></i>|]

quoteButton :: ByteString
quoteButton =
  let onClick :: Text
      onClick = [i|x-on:click="contentModel += '> '; $refs.contentRef.focus(); const pos = $refs.contentRef.value.length - 2; $refs.contentRef.setSelectionRange(pos, pos);"|]
   in [i|<i #{onClick} title='Quote' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-quote-left'></i>|]

codeButton :: ByteString
codeButton =
  let onClick :: Text
      onClick =
        [i|x-on:click="contentModel += '``';
$nextTick(() => {
  $refs.contentRef.focus();
  const pos = $refs.contentRef.value.length - 1;
  $refs.contentRef.setSelectionRange(pos, pos);
})"|]
   in [i|<i #{onClick} title='Code' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-code'></i>|]

urlButton :: ByteString
urlButton =
  let onClick :: Text
      onClick =
        [i|x-on:click="contentModel += '[](url)';
$nextTick(() => {
  $refs.contentRef.focus();
  const pos = $refs.contentRef.value.length - 6;
  $refs.contentRef.setSelectionRange(pos, pos);
})"|]
   in [i|<i #{onClick} title='Link' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-link'></i>|]

numberedListButton :: ByteString
numberedListButton =
  let onClick :: Text
      onClick =
        [i|x-on:click="
$refs.contentRef.focus();
const currentPos = $refs.contentRef.selectionStart;
const contentBeforeCursor = $refs.contentRef.value.substring(0, currentPos);
const rowStart = contentBeforeCursor.lastIndexOf('\\n') + 1;
contentModel = 
    $refs.contentRef.value.substring(0, rowStart) +
    '1. ' +
    $refs.contentRef.value.substring(rowStart);
"|]
   in [i|<i #{onClick} title='Numbered List' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-list-ol'></i>|]

unorderedListButton :: ByteString
unorderedListButton =
  let onClick :: Text
      onClick =
        [i|x-on:click="
$refs.contentRef.focus();
const currentPos = $refs.contentRef.selectionStart;
const contentBeforeCursor = $refs.contentRef.value.substring(0, currentPos);
const rowStart = contentBeforeCursor.lastIndexOf('\\n') + 1;
contentModel = 
    $refs.contentRef.value.substring(0, rowStart) +
    '- ' +
    $refs.contentRef.value.substring(rowStart);
"|]
   in [i|<i #{onClick} title='Unordered List' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-list'></i>|]

taskListButton :: ByteString
taskListButton =
  let onClick :: Text
      onClick =
        [i|x-on:click="
$refs.contentRef.focus();
const currentPos = $refs.contentRef.selectionStart;
const contentBeforeCursor = $refs.contentRef.value.substring(0, currentPos);
const rowStart = contentBeforeCursor.lastIndexOf('\\n') + 1;
contentModel = 
    $refs.contentRef.value.substring(0, rowStart) +
    '- [ ] ' +
    $refs.contentRef.value.substring(rowStart);
"|]
   in [i|<i #{onClick} title='Task List' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-list-check'></i>|]

contentField :: Maybe Text -> ByteString
contentField content =
  [i|
<div id='content-field' x-data="{ contentModel: '#{fold content}' }">
  <label for='content' class='mb-2 text-sm text-gray-900 font-semibold'>Add body</label>
  <div class='flex flex-col border border-gray-300 rounded-lg'>
      <div class='flex mb-2'>
          <div class='p-2 border-r border-gray-300 rounded-t-lg bg-white text-gray-900'>
            <button href='\#' role='tab' hx-get='/blog/new/edit' hx-swap='innerHTML' hx-target='\#content-field'>
              Write
            </button>
          </div>
  
          <div class='p-2 border-b border-gray-300 rounded-t-lg bg-gray-50 text-gray-500'>
            <button href='\#' role='tab' hx-get='/blog/new/preview' hx-swap='innerHTML' hx-target='\#content-field' hx-include='next textarea'>
              Preview
            </button>
          </div>
  
          <div class='p-2 border-b border-gray-300 rounded-t-lg bg-gray-50 text-gray-500 grow flex justify-end'>
              #{headingButton}
              #{boldButton}
              #{italicButton}
              <i class='border-l border-t-0 border-gray-300'></i>
              #{quoteButton}
              #{codeButton}
              #{urlButton}
              <i class='border-l border-t-0 border-gray-300'></i>
              #{numberedListButton}
              #{unorderedListButton}
              #{taskListButton}
          </div>
      </div>
      <div class='m-2 min-h-60'>
          <textarea name='content' x-model="contentModel" x-ref="contentRef" placeholder='Add your content here...' rows='11' class='block p-2.5 w-full text-sm text-gray-900 bg-gray-50 rounded-lg border border-gray-300 focus:ring-blue-500 focus:border-blue-500'>#{fold content}</textarea>
      </div>
  </div>
</div>
|]

--------------------------------------------------------------------------------

handler ::
  ( Applicative m,
    Has Trace.Tracer env,
    MonadCatch m,
    MonadUnliftIO m,
    MonadReader env m
  ) =>
  Auth.Authz ->
  Maybe Text ->
  m RawHtml
handler (Auth.Authz User.Domain {..} _) content =
  Observability.handlerSpan "GET /blog/new/edit" () display $ do
    unless dIsAdmin $ throwErr Unauthorized
    pageFragment <- parseFragment $ contentField content
    pure $ renderNodes pageFragment
