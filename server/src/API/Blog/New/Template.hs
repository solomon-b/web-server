{-# LANGUAGE QuasiQuotes #-}

module API.Blog.New.Template where

--------------------------------------------------------------------------------

import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Utils (escapeString)

--------------------------------------------------------------------------------

template :: ByteString
template =
  [i|
<div class='relative p-4 w-full max-w-4xl max-h-full mx-auto'>
  <div class='flex items-center justify-between p-4 md:p-5'>
      <h3 class='text-xl font-semibold text-gray-900'>Create Post</h3>
  </div>
  <div class='p-4 md:p-5'>
      <form hx-post='/blog/new' class='space-y-4 flex flex-col' data-bitwarden-watching='1' enctype="multipart/form-data">
          #{titleField}
          #{fileUploadField}
          #{contentField}
          #{submitButton}
      </form>
  </div>
</div>
|]

--------------------------------------------------------------------------------

publishToggle :: ByteString
publishToggle =
  [i|
<div class='px-5 font-medium inline-flex'>
<label class='inline-flex items-center cursor-pointer'>
  <span class='px-2.5 ms-3 text-sm font-medium text-gray-900 font-semibold'>Published</span>
  <input type='checkbox' name='published' class='sr-only peer' value='true' />
  <div class="relative w-11 h-6 bg-gray-200 peer-focus:outline-none peer-focus:ring-4 peer-focus:ring-green-300 rounded-full peer peer-checked:after:translate-x-full rtl:peer-checked:after:-translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:start-[2px] after:bg-white after:border-gray-300 after:border after:rounded-full after:w-5 after:h-5 after:transition-all peer-checked:bg-green-600"></div>
</label>
</div>
|]

--------------------------------------------------------------------------------

titleField :: ByteString
titleField =
  [i|
<div>
  <label for='title' class='mb-2 text-sm text-gray-900 font-semibold'>Add title</label>
  <input type='text' placeholder='title' name='title' id='title' class='bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-green-500 focus:border-green-500 block w-full p-2.5' />
</div>
|]

--------------------------------------------------------------------------------

fileUploadField :: ByteString
fileUploadField =
  [i|
<div>
  <label class="block mb-2 text-sm font-medium text-gray-900 font-semibold" for="hero_image_input">Upload Hero Image</label>
  <input type="file" name="heroImagePath" class="block w-full text-sm text-gray-900 border border-gray-300 rounded-lg cursor-pointer bg-gray-50 focus:outline-none" id="hero_image_input">
</div>
|]

--------------------------------------------------------------------------------

submitButton :: ByteString
submitButton =
  [i|
<div class='flex justify-end'>
  #{publishToggle}
  <button id='publishedStatusButton' type='submit' class='text-white bg-green-700 hover:bg-green-800 focus:ring-4 focus:outline-none focus:ring-green-300 font-medium rounded-lg text-sm px-5 py-2.5 text-center inline-flex items-center'>
    Create Post
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

contentFieldFooter :: ByteString
contentFieldFooter =
  [i|
<div class="flex text-xs text-gray-800">
  <div class="p-1">
    <a href="https://daringfireball.net/projects/markdown/syntax" target="_blank" class="">
      <span class="flex rounded-lg hover:bg-gray-200 p-2">
        <span class="px-1">
          <svg aria-hidden="true" height="16" viewBox="0 0 16 16" version="1.1" width="16" class="octicon octicon-markdown">
            <path d="M14.85 3c.63 0 1.15.52 1.14 1.15v7.7c0 .63-.51 1.15-1.15 1.15H1.15C.52 13 0 12.48 0 11.84V4.15C0 3.52.52 3 1.15 3ZM9 11V5H7L5.5 7 4 5H2v6h2V8l1.5 1.92L7 8v3Zm2.99.5L14.5 8H13V5h-2v3H9.5Z"></path>
          </svg>
        </span>
        <span class="Button-label">Markdown is supported</span>
      </span>
    </a>
  </div>
  <i class='border-l border-t-0 border-gray-300 my-2'></i>
  <div class="p-1" x-data="fileUpload">
    <button type='button' @click="selectAndUpload">
      <span class="flex p-2 rounded-lg hover:bg-gray-200">
        <span class="px-1">
          <svg height="16" viewBox="0 0 16 16" version="1.1" width="16" data-view-component="true" class="octicon octicon-image">
            <path d="M16 13.25A1.75 1.75 0 0 1 14.25 15H1.75A1.75 1.75 0 0 1 0 13.25V2.75C0 1.784.784 1 1.75 1h12.5c.966 0 1.75.784 1.75 1.75ZM1.75 2.5a.25.25 0 0 0-.25.25v10.5c0 .138.112.25.25.25h.94l.03-.03 6.077-6.078a1.75 1.75 0 0 1 2.412-.06L14.5 10.31V2.75a.25.25 0 0 0-.25-.25Zm12.5 11a.25.25 0 0 0 .25-.25v-.917l-4.298-3.889a.25.25 0 0 0-.344.009L4.81 13.5ZM7 6a2 2 0 1 1-3.999.001A2 2 0 0 1 7 6ZM5.5 6a.5.5 0 1 0-1 0 .5.5 0 0 0 1 0Z"></path>
          </svg>
        </span>
        <span class="Button-label">Click to add files</span>
      </span>
    </button>
    <input type="file" x-ref="fileInput" @change="uploadFile" style="display: none;">
  </div>
</div>

<script>
  function fileUpload() {
    return {
      file: null,
      fileName: '',
      selectAndUpload() {
        // Open the file picker
        this.$refs.fileInput.click(); 
      },
      async uploadFile(event) {
        const files = event.target.files;

        this.file = files[0];
        this.fileName = this.file.name;

        const formData = new FormData();
        formData.append('title', this.fileName);
        formData.append('file', this.file);

        try {
          const response = await fetch('/image/new', {
            method: 'POST',
            body: formData,
          });

          if (response.ok) {
            const {url} = await response.json();

            this.contentModel += `![${this.fileName}](${url})\n`
            this.$refs.contentRef.focus();
            this.$nextTick(() => {
              this.$refs.contentRef.focus();
            });
          } else {
            console.log(`Upload failed: ${response.statusText}`);
          }
        } catch (error) {
          console.log(`Error: ${error.message}`);
        }
      },
    };
  } 
</script>
|]

--------------------------------------------------------------------------------

contentFieldEdit :: ByteString
contentFieldEdit =
  [i|
<label for='content' class='mb-2 text-sm text-gray-900 font-semibold'>Add body</label>
<div class='flex flex-col border border-gray-300 rounded-lg'>
    <div class='flex mb-2'>
        <div class='p-2 border-r border-gray-300 rounded-t-lg bg-white text-gray-900'>
          <button href='\#' role='tab' hx-get='/blog/new/edit' hx-swap='innerHTML' hx-target='\#content-field' @click="console.log(contentModel)">
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
        <textarea name='content' x-model="contentModel" x-ref="contentRef" placeholder='Add your content here...' rows='11' class='block p-2.5 w-full text-sm text-gray-900 bg-gray-50 rounded-lg border border-gray-300 focus:ring-blue-500 focus:border-blue-500'></textarea>
    </div>
    #{contentFieldFooter}
</div>
|]

emptyPreview :: Text
emptyPreview = "<p name='content' rows='8' class='p-2 w-full text-sm text-gray-900'>Nothing to preview</p>"

contentFieldPreview :: Maybe Text -> ByteString
contentFieldPreview content =
  [i|
<label for='content' class='mb-2 text-sm text-gray-900 font-semibold'>Add body</label>
<div class='flex flex-col border rounded-lg border-gray-300'>
    <div class='flex mb-2'>
        <div class='p-2 border-b rounded-t-lg border-gray-300 text-gray-500 bg-gray-50'>
          <button role='tab' hx-get="/blog/new/edit" hx-swap="innerHTML" hx-target="\#content-field">
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

contentField :: ByteString
contentField =
  [i|
<div id='content-field' x-data="{ contentModel: '' }">
  #{contentFieldEdit}
</div>
|]
