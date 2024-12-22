{-# LANGUAGE QuasiQuotes #-}

module API.Blog.Id.Template where

--------------------------------------------------------------------------------

import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Foldable (fold)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Utils (escapeString)

--------------------------------------------------------------------------------

template :: BlogPosts.Id -> BlogPosts.Subject -> BlogPosts.Body -> Bool -> Maybe Text -> ByteString
template bid title content isPublished heroImagePath =
  [i|
<div class='relative p-4 w-full max-w-4xl max-h-full mx-auto'>
  <div class='flex items-center justify-between p-4 md:p-5'>
      <h3 class='text-xl font-semibold text-gray-900'>Create Post</h3>
  </div>
  <div class='p-4 md:p-5'>
      <form hx-post='/blog/#{bid}/edit' class='space-y-4 flex flex-col' data-bitwarden-watching='1' enctype="multipart/form-data">
          #{titleField (display title)}
          #{fileUploadField heroImagePath}
          #{contentField bid content}
          #{submitButton isPublished}
      </form>
  </div>
</div>
|]

--------------------------------------------------------------------------------

publishToggle :: Bool -> ByteString
publishToggle isPublished =
  let checked :: Text
      checked = bool "" "checked" isPublished
   in [i|
<div class='px-5 font-medium inline-flex'>
<label class='inline-flex items-center cursor-pointer'>
  <span class='px-2.5 ms-3 text-sm font-medium text-gray-900 font-semibold'>Published</span>
  <input type='checkbox' name='published' class='sr-only peer' value='true' #{checked} />
  <div class="relative w-11 h-6 bg-gray-200 peer-focus:outline-none peer-focus:ring-4 peer-focus:ring-green-300 rounded-full peer peer-checked:after:translate-x-full rtl:peer-checked:after:-translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:start-[2px] after:bg-white after:border-gray-300 after:border after:rounded-full after:w-5 after:h-5 after:transition-all peer-checked:bg-green-600"></div>
</label>
</div>
|]

--------------------------------------------------------------------------------

titleField :: Text -> ByteString
titleField title =
  [i|
<div>
  <label for='title' class='mb-2 text-sm text-gray-900 font-semibold'>Add title</label>
  <input type='text' placeholder='title' name='title' id='title' value='#{title}' class='bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-green-500 focus:border-green-500 block w-full p-2.5' />
</div>
|]

--------------------------------------------------------------------------------

fileUploadField :: Maybe Text -> ByteString
fileUploadField _heroImagePath =
  [i|
<div>
  <label class="block mb-2 text-sm font-medium text-gray-900 font-semibold" for="hero_image_input">Upload Hero Image</label>
  <input type="file" name="heroImagePath" class="block w-full text-sm text-gray-900 border border-gray-300 rounded-lg cursor-pointer bg-gray-50 focus:outline-none" id="hero_image_input">
</div>
|]

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

contentFieldEdit :: BlogPosts.Id -> BlogPosts.Body -> ByteString
contentFieldEdit bid content =
  [i|  
<label for='content' class='mb-2 text-sm text-gray-900 font-semibold'>Add body</label>
<div class='flex flex-col border border-gray-300 rounded-lg'>
    <div class='flex mb-2'>
        <div class='p-2 border-r border-gray-300 rounded-t-lg bg-white text-gray-900'>
          <button role='tab' hx-get="/blog/#{display bid}/edit" hx-swap="innerHTML" hx-target="\#content-field" hx-include='next textarea'>
            Write
          </button>
        </div>

        <div class='p-2 border-b border-gray-300 rounded-t-lg bg-gray-50 text-gray-500'>
          <button href='\#' role='tab' hx-get='/blog/#{display bid}/preview' hx-swap='innerHTML' hx-target='\#content-field' hx-include='next textarea'>
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
        <textarea name='content' x-model="contentModel" x-ref="contentRef" placeholder='Add your content here...' rows='11' class='block p-2.5 w-full text-sm text-gray-900 bg-gray-50 rounded-lg border border-gray-300 focus:ring-blue-500 focus:border-blue-500'>#{content}</textarea>
    </div>
</div>
|]

emptyPreview :: Text
emptyPreview = "<p name='content' rows='8' class='p-2 w-full text-sm text-gray-900'>Nothing to preview</p>"

contentFieldPreview :: BlogPosts.Id -> Maybe Text -> ByteString
contentFieldPreview bid content =
  [i|
<label for='content' class='mb-2 text-sm text-gray-900 font-semibold'>Add body</label>
<div class='flex flex-col border rounded-lg border-gray-300'>
    <div class='flex mb-2'>
        <div class='p-2 border-b rounded-t-lg border-gray-300 text-gray-500 bg-gray-50'>
          <button role='tab' hx-get="/blog/#{display bid}/edit" hx-swap="innerHTML" hx-target="\#content-field" hx-vals='{"content": "#{fold content}"}'>
            Write
          </button>
        </div>

        <div class='p-2 border-x rounded-t-lg border-gray-300 text-gray-900 bg-white'>
          <button role='tab' hx-get="/blog/new/preview" hx-swap="innerHTML" hx-target="\#content-field" hx-include='{"content": "#{escapeString $ fromMaybe emptyPreview content}"}'>
            Preview
          </button>
        </div>

        <div class='p-2 border-b rounded-t-lg border-gray-300 text-gray-500 bg-gray-50 grow flex justify-end'>
        </div>
    </div>
    <div class='m-2 min-h-60'>#{fromMaybe emptyPreview content}</div>
</div>
|]

contentField :: BlogPosts.Id -> BlogPosts.Body -> ByteString
contentField bid content =
  [i|
<div id='content-field' x-data="{ contentModel: '#{content}'}">
  #{contentFieldEdit bid content}
</div>
|]
