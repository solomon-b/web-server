{-# LANGUAGE QuasiQuotes #-}

module API.Blog.New.Template where

--------------------------------------------------------------------------------

import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.InvalidField (InvalidField)
import Effects.Database.Tables.BlogPosts qualified as BlogPost

--------------------------------------------------------------------------------

template ::
  Maybe BlogPost.Id ->
  Maybe BlogPost.Subject ->
  Maybe BlogPost.Body ->
  Bool ->
  Maybe Text ->
  [InvalidField] ->
  ByteString
template bid subject body isPublished heroImagePath invalidFields =
  let postPath :: Text
      postPath = maybe "/blog/new" (const [i|/blog/#{display bid}/edit|]) bid
   in [i|
<div class='relative p-4 w-full max-w-4xl max-h-full mx-auto'>
  <div class='flex items-center justify-between p-4 md:p-5'>
      <h3 class='text-xl font-semibold text-gray-900'>Create Post</h3>
  </div>
  <div class='p-4 md:p-5' x-data="{ editMode: true, fields: { subject: { value: '', isValid: true }, body: { value: '', isValid: true }}}">
      <form hx-post='#{postPath}' class='space-y-4 flex flex-col' data-bitwarden-watching='1' enctype="multipart/form-data">
          #{titleField ("Subject" `elem` invalidFields) subject}
          #{fileUploadField heroImagePath}
          #{contentField bid ("Body" `elem` invalidFields) body}
          #{submitButton isPublished}
      </form>
      #{javascript}
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

titleField :: Bool -> Maybe BlogPost.Subject -> ByteString
titleField _isInvalid subject =
  let inputValue = foldMap display subject
   in [i|
<div x-data="handler">
  <label for='title' class='mb-2 text-sm text-gray-900 font-semibold'>
    Add title
    <span class="text-xs text-red-900" x-bind:hidden="fields.subject.isValid" hidden> * required</span>
  </label>
  <input
    type='text'
    id='title'
    name='title'
    placeholder='title'
    value='#{inputValue}'
    class='border text-gray-900 text-sm rounded-lg block w-full p-2.5'
    x-model.lazy="fields.subject.value"
    :class="fields.subject.isValid ? 'bg-gray-50 border-gray-300 focus:ring-green-500 focus:border-green-500' : 'bg-red-50 border-red-900 focus:ring-red-500 focus:border-red-500'"
    @blur="validateField('subject')"
  />
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
<div class='flex justify-end'  x-data="handler">
  #{publishToggle isPublished}
  <button
    type='submit'
    id='publishedStatusButton'
    class='text-white bg-green-700 hover:bg-green-800 focus:ring-4 focus:outline-none focus:ring-green-300 font-medium rounded-lg text-sm px-5 py-2.5 text-center inline-flex items-center'
    :disabled="!allValid()"
    disabled
  >
    Create Post
  </button>
</div>
|]

--------------------------------------------------------------------------------

headingButton :: ByteString
headingButton =
  let onClick :: Text
      onClick = [i|x-on:click="fields.body.value += '\#\#\# '; $refs.contentRef.focus()"|]
   in [i|<i #{onClick} title='Heading' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-heading'></i>|]

boldButton :: ByteString
boldButton =
  let onClick :: Text
      onClick = [i|x-on:click="surroundFocus($refs.contentRef, '**')"|]
   in [i|<i #{onClick} title='Bold' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-bold'></i>|]

italicButton :: ByteString
italicButton =
  let onClick :: Text
      onClick = [i|x-on:click="surroundFocus($refs.contentRef, '_')"|]
   in [i|<i #{onClick} title='Italic' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-italic'></i>|]

quoteButton :: ByteString
quoteButton =
  let onClick :: Text
      onClick = [i|x-on:click="togglePrefix($refs.contentRef, '> ')"|]
   in [i|<i #{onClick} title='Quote' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-quote-left'></i>|]

codeButton :: ByteString
codeButton =
  let onClick :: Text
      onClick =
        [i|x-on:click="fields.body.value += '``';
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
        [i|x-on:click="fields.body.value += '[](url)';
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
fields.body.value = 
    $refs.contentRef.value.substring(0, rowStart) +
    '1. ' +
    $refs.contentRef.value.substring(rowStart);
"|]
   in [i|<i #{onClick} title='Numbered List' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-list-ol'></i>|]

unorderedListButton :: ByteString
unorderedListButton =
  let onClick :: Text
      onClick = [i|x-on:click="togglePrefix($refs.contentRef, '- ')"|]
   in [i|<i #{onClick} title='Unordered List' class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-list'></i>|]

taskListButton :: ByteString
taskListButton =
  let onClick :: Text
      onClick = [i|x-on:click="togglePrefix($refs.contentRef, '- [ ] ')"|]
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
  <div class="p-1">
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
|]

--------------------------------------------------------------------------------

contentFieldEdit :: Maybe BlogPost.Id -> Maybe BlogPost.Body -> Bool -> ByteString
contentFieldEdit _bid _content _isInvalid =
   [i|
<div id="contentEdit" x-bind:hidden="!editMode">
  <div class='flex mb-2'>
      <div class='p-2 border-r border-gray-300 rounded-t-lg bg-white text-gray-900'>
            <button type='button' @click="editMode = true">
          Write
        </button>
      </div>
  
      <div class='p-2 border-b border-gray-300 rounded-t-lg bg-gray-50 text-gray-500'>
            <button type='button' @click="editMode = false">
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
      <textarea
        name='content'
        placeholder='Add your content here...'
        rows='11'
        class='block p-2.5 w-full text-sm rounded-lg border'
        x-model.lazy="fields.body.value"
        x-ref="contentRef"
        :class="fields.body.isValid ? 'bg-gray-50 border-gray-300 focus:ring-green-500 focus:border-green-500' : 'bg-red-50 border-red-900 focus:ring-red-500 focus:border-red-500'"
        @blur="validateField('body')"
      >
      </textarea>
  </div>
  #{contentFieldFooter}
</div>
|]

emptyPreview :: Text
emptyPreview = "<p name='content' rows='8' class='p-2 w-full text-sm text-gray-900'>Nothing to preview</p>"

contentFieldPreview :: ByteString
contentFieldPreview =
  [i|
<div id="contentPreview" x-bind:hidden="editMode">
    <div class='flex mb-2'>
        <div class='p-2 border-b rounded-t-lg border-gray-300 text-gray-500 bg-gray-50'>
          <button type='button' @click="editMode = true">
            Write
          </button>
        </div>

        <div class='p-2 border-x border-t rounded-t-lg border-gray-300 text-gray-900 bg-white'>
          <button type='button' @click="editMode = false">
            Preview
          </button>
        </div>

        <div class='p-2 border-b rounded-t-lg border-gray-300 text-gray-500 bg-gray-50 grow flex justify-end'>
        </div>
    </div>
    <div class='m-3 min-h-60' x-text='fields.body.value'></div>
</div>
|]

contentField :: Maybe BlogPost.Id -> Bool -> Maybe BlogPost.Body -> ByteString
contentField bid isInvalid body =
  [i|
<div id='content-field'>
  <label for='content' class='mb-2 text-sm text-gray-900 font-semibold'>
    Add body
    <span class="text-xs text-red-900" x-bind:hidden="fields.body.isValid" hidden> * required</span>
  </label>
  <div class='flex flex-col border border-gray-300 rounded-lg' x-data="handler">
    #{contentFieldEdit bid body isInvalid}
    #{contentFieldPreview}
  </div>
</div>
|]

-- TODO: Move this into a js file:
javascript :: ByteString
javascript =
  [i|
<script>
  function handler() {
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
            const urlWithScheme = url.startsWith("http") ? url : "http://" + url
            const {pathname} = new URL(urlWithScheme);
            
            this.fields.body.value += `![${this.fileName}](${pathname})\n`
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
      togglePrefix(textarea, prefix) {
        textarea.focus();

        if (!prefix) {
          throw new Error('The "prefix" parameter is required.');
        }

        const expandSelectionToFullLines = (content, selectionStart, selectionEnd) => {
          while (selectionStart > 0 && content[selectionStart - 1] !== '\\n') {
            selectionStart--;
          }
          while (selectionEnd < content.length && content[selectionEnd] !== '\\n') {
            endPos++;
          }
          return {selectionStart, selectionEnd};
        };

        const processLines = (lines, prefix) => {
          const allLinesPrefixed = lines.every(line => line.startsWith(prefix));
          if (allLinesPrefixed) {
            const removePrefix = (xs) => xs.map(line => line.startsWith(prefix) ? line.substring(prefix.length) : line);
            return  removePrefix(lines);
          } else {
            const addPrefix = (xs) => xs.map(line => (line.startsWith(prefix) ? line : prefix + line)); 
            return addPrefix(lines)
          }
        };

        const reassembleContent = (content, selectionStart, selectionEnd, updatedLines) => {
          const beforeSelection = content.substring(0, selectionStart);
          const afterSelection = content.substring(selectionEnd);
          return beforeSelection + updatedLines.join('\\n') + afterSelection;
        };

        const updateTextarea = (textarea, newContent, selectionStart, selectionEnd) => {
          textarea.setSelectionRange(0, textarea.value.length); // Select all
          document.execCommand('insertText', false, newContent); // Update with undo support
          textarea.setSelectionRange(selectionStart, selectionEnd); // Restore selection
        };

        const content = textarea.value;
        let {selectionStart, selectionEnd} = expandSelectionToFullLines(content, textarea.selectionStart, textarea.selectionEnd);

        const selection = content.substring(selectionStart, selectionEnd);
        const lines = selection.split('\\n');
        const updatedLines = processLines(lines, prefix);
        const updatedContent = reassembleContent(content, selectionStart, selectionEnd, updatedLines);
        const adjustedEndPos = selectionStart + updatedLines.join('\\n').length;

        updateTextarea(textarea, updatedContent, selectionStart, adjustedEndPos);
      },
      surroundFocus(textarea, wrapper) {
        textarea.focus();
      
        // Split text into a Zipper.
        const splitText = (text, start, end) => ({
          before: text.slice(0, start),
          selection: text.slice(start, end),
          after: text.slice(end),
        });
      
        const isWrapped = (text, wrapper) =>
          text.startsWith(wrapper) && text.endsWith(wrapper);
      
        const escapeRegex = (text) => text.replace(/[-/\\\\^$*+?.()|[\\]{}]/g, '\\\\$&');
      
        // Remove any `**` inside the selection but keep the inner content
        const cleanInnerWrappers = (text, wrapper) => {
          const escapedWrapper = escapeRegex(wrapper);
          const regex = new RegExp(`${escapedWrapper}(.*?)${escapedWrapper}`, 'g');
          return text.replace(regex, '$1'); 
        };
      
        // Replace the content of the textarea respecting undo history.
        const insertText = (textarea, newValue) => {
          textarea.setSelectionRange(0, textarea.value.length);
          document.execCommand('insertText', false, newValue);
        };
      
        // Constants
        const { selectionStart: start, selectionEnd: end } = textarea;
        const { before, selection, after } = splitText(
          textarea.value,
          start,
          end
        );
      
        const isSelected = start !== end;
      
        // Determine the updated text and cursor/selection range
        let updatedValue, newStart, newEnd;
      
        if (isSelected) {
          if (isWrapped(selection, wrapper)) {
            // Remove wrapping if fully wrapped
            const unwrapped = selection.slice(wrapper.length, -wrapper.length);
            updatedValue = before + unwrapped + after;
            newStart = start;
            newEnd = start + unwrapped.length;
          } else {
            // Clean inner wrappers and wrap the selection
            const cleaned = cleanInnerWrappers(selection, wrapper);
            const wrapped = `${wrapper}${cleaned}${wrapper}`;
            updatedValue = before + wrapped + after;
            newStart = start;
            newEnd = start + wrapped.length;
          }
        } else {
          const hasOuterWrappers =
            isWrapped(before.slice(-wrapper.length) + after.slice(0, wrapper.length), wrapper);
      
          if (hasOuterWrappers) {
            // Remove outer wrappers
            updatedValue = before.slice(0, -wrapper.length) + after.slice(wrapper.length);
            newStart = newEnd = start - wrapper.length;
          } else {
            // Insert `${wrapper}${wrapper}` at the cursor and position the cursor between them
            updatedValue = before + wrapper + wrapper + after;
            newStart = newEnd = start + wrapper.length;
          }
        }
      
        // Update the textarea and adjust the selection
        insertText(textarea, updatedValue);
        textarea.setSelectionRange(newStart, newEnd);
      },
      validateField (field) {
         this.fields[field].isValid = this.fields[field].value.trim() !== '';
      },
      allValid() {
        return Object.values(this.fields).every(field => field.isValid && field.value.trim() !== '');
      },
    };
  }
</script>
|]
