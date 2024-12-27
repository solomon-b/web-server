{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}

module Component.Forms.BlogPost where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (markdownPostLink)
import Data.Maybe (catMaybes)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Effects.Database.Tables.BlogPosts qualified as BlogPost
import Lucid (a_, button_, checked_, class_, disabled_, div_, enctype_, for_, form_, h3_, height_, hidden_, href_, i_, id_, input_, label_, name_, placeholder_, rows_, script_, span_, style_, svg_, target_, textarea_, title_, type_, value_, width_)
import Lucid qualified
import Lucid.Extras
import Web.HttpApiData qualified as Http

--------------------------------------------------------------------------------

markdownPostUrl :: Text
markdownPostUrl = "/" <> Http.toUrlPiece markdownPostLink

template ::
  Maybe BlogPost.Id ->
  Maybe BlogPost.Subject ->
  Maybe BlogPost.Body ->
  Bool ->
  Maybe Text ->
  Lucid.Html ()
template bid subject body isPublished heroImagePath =
  let postPath :: Text
      postPath = maybe "/blog/new" (\bid' -> [i|/blog/#{display bid'}/edit|]) bid
      subject' = maybe "" display subject
      body' = maybe "" display body
   in div_ [class_ "relative p-4 w-full max-w-4xl max-h-full mx-auto"] do
        div_ [class_ "flex items-center justify-between p-4 md:p-5"] do
          h3_ [class_ "text-xl font-semibold text-gray-900"] do
            "Create Post"

        div_ [class_ "p-4 md:p-5", xData_ [i|{ editMode: true, fields: { subject: { value: '#{subject'}', isValid: true }, body: { value: `#{body'}`, isValid: true, valueParsed: '' }}}|]] do
          form_ [hxPost_ postPath, class_ "space-y-4 flex flex-col", enctype_ "multipart/form-data"] do
            titleField
            fileUploadField heroImagePath
            contentField
            submitButton isPublished
          javascript

--------------------------------------------------------------------------------

publishToggle :: Bool -> Lucid.Html ()
publishToggle isPublished =
  div_ [class_ "px-5 font-medium inline-flex'"] do
    label_ [class_ "inline-flex items-center cursor-pointer"] do
      span_ [class_ "px-2.5 ms-3 text-sm font-medium text-gray-900 font-semibold"] do
        "Published"
      input_ ([type_ "checkbox", name_ "published", class_ "sr-only peer", value_ "true"] <> catMaybes [if isPublished then Just checked_ else Nothing])
      div_ [class_ "relative w-11 h-6 bg-gray-200 peer-focus:outline-none peer-focus:ring-4 peer-focus:ring-green-300 rounded-full peer peer-checked:after:translate-x-full rtl:peer-checked:after:-translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:start-[2px] after:bg-white after:border-gray-300 after:border after:rounded-full after:w-5 after:h-5 after:transition-all peer-checked:bg-green-600"] mempty

--------------------------------------------------------------------------------

titleField :: Lucid.Html ()
titleField =
  div_ [xData_ "alpineHandler"] do
    label_ [for_ "title", class_ "mb-2 text-sm text-gray-900 font-semibold"] do
      "Add title"
      span_ [class_ "text-xs text-red-900", xBindHidden_ "fields.subject.isValid", hidden_ "true"] do
        " * required"
    input_
      [ type_ "text",
        id_ "title",
        name_ "title",
        placeholder_ "title",
        class_ "border text-gray-900 text-sm rounded-lg block w-full p-2.5",
        xModel_ "fields.subject.value",
        xBindClass_ "fields.subject.isValid ? 'bg-gray-50 border-gray-300 focus:ring-green-500 focus:border-green-500' : 'bg-red-50 border-red-900 focus:ring-red-500 focus:border-red-500'",
        xOnBlur_ "validateField('subject')",
        xOnInput_ "validateField('subject')"
      ]

--------------------------------------------------------------------------------

fileUploadField :: Maybe Text -> Lucid.Html ()
fileUploadField _heroImagePath =
  div_ do
    label_
      [class_ "block mb-2 text-sm font-medium text-gray-900 font-semibold", for_ "hero_image_input"]
      "Upload Hero Image"
    input_ [type_ "file", name_ "heroImagePath", class_ "block w-full text-sm text-gray-900 border border-gray-300 rounded-lg cursor-pointer bg-gray-50 focus:outline-none", id_ "hero_image_input"]

--------------------------------------------------------------------------------

submitButton :: Bool -> Lucid.Html ()
submitButton isPublished =
  div_ [class_ "flex justify-end", xData_ "alpineHandler"] do
    publishToggle isPublished
    button_
      [ type_ "submit",
        id_ "publishedStatusButton",
        class_ "text-white bg-green-700 hover:bg-green-800 focus:ring-4 focus:outline-none focus:ring-green-300 font-medium rounded-lg text-sm px-5 py-2.5 text-center inline-flex items-center",
        xBindDisabled_ "!allValid()",
        disabled_ "true"
      ]
      "Create Post"

--------------------------------------------------------------------------------

headingButton :: Lucid.Html ()
headingButton =
  i_ [xOnClick_ "togglePrefix($refs.contentRef, '### ')", title_ "Heading", class_ "p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-heading"] mempty

boldButton :: Lucid.Html ()
boldButton =
  i_ [xOnClick_ "surroundFocus($refs.contentRef, '**')", title_ "Bold", class_ "p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-bold"] mempty

italicButton :: Lucid.Html ()
italicButton =
  i_ [xOnClick_ "surroundFocus($refs.contentRef, '_')", title_ "Italic", class_ "p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-italic"] mempty

quoteButton :: Lucid.Html ()
quoteButton =
  i_ [xOnClick_ "togglePrefix($refs.contentRef, '> ')", title_ "Quote", class_ "p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-quote-left"] mempty

codeButton :: Lucid.Html ()
codeButton =
  i_ [xOnClick_ "surroundFocus($refs.contentRef, '`')", title_ "Code", class_ "p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-code"] mempty

urlButton :: Lucid.Html ()
urlButton =
  i_ [xOnClick_ "insertUrl($refs.contentRef)", title_ "Link", class_ "p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-link"] mempty

numberedListButton :: Lucid.Html ()
numberedListButton =
  i_ [xOnClick_ "togglePrefix($refs.contentRef, '1. ')", title_ "Numbered List", class_ "p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-list-ol"] mempty

unorderedListButton :: Lucid.Html ()
unorderedListButton =
  i_ [xOnClick_ "togglePrefix($refs.contentRef, '- ')", title_ "Unordered List", class_ "p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-list"] mempty

taskListButton :: Lucid.Html ()
taskListButton =
  i_ [xOnClick_ "togglePrefix($refs.contentRef, '- [ ] ')", title_ "Task List", class_ "p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-list-check"] mempty

--------------------------------------------------------------------------------

contentFieldFooter :: Lucid.Html ()
contentFieldFooter =
  div_ [class_ "flex text-xs text-gray-800"] $ do
    div_ [class_ "p-1"] do
      a_ [href_ "https://daringfireball.net/projects/markdown/syntax", target_ "_blank", class_ ""] do
        span_ [class_ "flex rounded-lg hover:bg-gray-200 p-2"] do
          span_ [class_ "px-1"] do
            svg_ [ariaHidden_ "true", height_ "16", viewBox_ "0 0 16 16", version_ "1.1", width_ "16", class_ "octicon octicon-markdown"] do
              path_ [d_ "M14.85 3c.63 0 1.15.52 1.14 1.15v7.7c0 .63-.51 1.15-1.15 1.15H1.15C.52 13 0 12.48 0 11.84V4.15C0 3.52.52 3 1.15 3ZM9 11V5H7L5.5 7 4 5H2v6h2V8l1.5 1.92L7 8v3Zm2.99.5L14.5 8H13V5h-2v3H9.5Z"] mempty
          span_ [class_ "Button-label"] do
            "Markdown is supported"
    i_ [class_ "border-l border-t-0 border-gray-300 my-2"] mempty
    div_ [class_ "p-1"] do
      button_ [type_ "button", xOnClick_ "selectAndUpload"] do
        span_ [class_ "flex p-2 rounded-lg hover:bg-gray-200"] do
          span_ [class_ "px-1"] do
            svg_ [height_ "16", viewBox_ "0 0 16 16", version_ "1.1", width_ "16", dataViewComponent_ "true", class_ "octicon octicon-image"] do
              path_ [d_ "M16 13.25A1.75 1.75 0 0 1 14.25 15H1.75A1.75 1.75 0 0 1 0 13.25V2.75C0 1.784.784 1 1.75 1h12.5c.966 0 1.75.784 1.75 1.75ZM1.75 2.5a.25.25 0 0 0-.25.25v10.5c0 .138.112.25.25.25h.94l.03-.03 6.077-6.078a1.75 1.75 0 0 1 2.412-.06L14.5 10.31V2.75a.25.25 0 0 0-.25-.25Zm12.5 11a.25.25 0 0 0 .25-.25v-.917l-4.298-3.889a.25.25 0 0 0-.344.009L4.81 13.5ZM7 6a2 2 0 1 1-3.999.001A2 2 0 0 1 7 6ZM5.5 6a.5.5 0 1 0-1 0 .5.5 0 0 0 1 0Z"] mempty
          span_ [class_ "Button-label"] do
            "Click to add files"
      input_ [type_ "file", xRef_ "fileInput", xOnChange_ "uploadFile", style_ "display: none;"]

--------------------------------------------------------------------------------

contentFieldEdit :: Lucid.Html ()
contentFieldEdit =
  div_ [id_ "contentEdit", xBindHidden_ "!editMode"] do
    div_ [class_ "flex mb-2"] do
      div_ [class_ "p-2 border-r border-gray-300 rounded-t-lg bg-white text-gray-900"] do
        button_
          [type_ "button", xOnClick_ "editMode = true"]
          "Write"

      div_ [class_ "p-2 border-b border-gray-300 rounded-t-lg bg-gray-50 text-gray-500"] do
        button_
          [type_ "button", xOnClick_ "loadPreview"]
          "Preview"

      div_ [class_ "p-2 border-b border-gray-300 rounded-t-lg bg-gray-50 text-gray-500 grow flex justify-end"] do
        headingButton
        boldButton
        italicButton
        i_ [class_ "border-l border-t-0 border-gray-300"] mempty
        quoteButton
        codeButton
        urlButton
        i_ [class_ "border-l border-t-0 border-gray-300"] mempty
        numberedListButton
        unorderedListButton
        taskListButton

    div_ [class_ "m-2 min-h-60"] do
      textarea_
        [ name_ "content",
          placeholder_ "Add your content here...",
          rows_ "11",
          class_ "block p-2.5 w-full text-sm rounded-lg border",
          xModel_ "fields.body.value",
          xRef_ "contentRef",
          xBindClass_ "fields.body.isValid ? 'bg-gray-50 border-gray-300 focus:ring-green-500 focus:border-green-500' : 'bg-red-50 border-red-900 focus:ring-red-500 focus:border-red-500'",
          xOnBlur_ "validateField('body')",
          xOnInput_ "validateField('body')"
        ]
        mempty
    contentFieldFooter

contentFieldPreview :: Lucid.Html ()
contentFieldPreview =
  div_ [xBindHidden_ "editMode"] do
    div_ [class_ "flex mb-2"] do
      div_ [class_ "p-2 border-b rounded-t-lg border-gray-300 text-gray-500 bg-gray-50"] do
        button_
          [type_ "button", xOnClick_ "editMode = true"]
          "Write"

      div_ [class_ "p-2 border-x border-t rounded-t-lg border-gray-300 text-gray-900 bg-white"] do
        button_
          [type_ "button", xOnClick_ "loadPreview"]
          "Preview"

      div_ [class_ "p-2 border-b rounded-t-lg border-gray-300 text-gray-500 bg-gray-50 grow flex justify-end"] mempty
    div_ [id_ "contentPreview", class_ "m-3 min-h-60", xHtml_ "fields.body.valueParsed"] mempty

contentField :: Lucid.Html ()
contentField =
  div_ [id_ "content-field"] do
    label_ [for_ "content", class_ "mb-2 text-sm text-gray-900 font-semibold"] do
      "Add body"
      span_
        [class_ "text-xs text-red-900", xBindHidden_ "fields.body.isValid", hidden_ "true"]
        " * required"
    div_ [class_ "flex flex-col border border-gray-300 rounded-lg", xData_ "alpineHandler"] do
      contentFieldEdit
      contentFieldPreview

-- TODO: Move this into a js file:
javascript :: Lucid.Html ()
javascript =
  Lucid.script_
    [i|
  async function uploadFile (event) {
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
  };

  /------------------------------------------------------------------------------/

  function insertUrl (textarea) {
    textarea.focus();

    const spliceContent = (content, splicePoint, updatedLines) => {
      const beforeSelection = content.substring(0, splicePoint);
      const afterSelection = content.substring(splicePoint);
      return beforeSelection + '[](url)' + afterSelection;
    };

    const updateTextarea = (textarea, newContent, splicePoint) => {
      textarea.setSelectionRange(0, textarea.value.length); // Select all
      document.execCommand('insertText', false, newContent); // Update with undo support
      textarea.setSelectionRange(splicePoint + 1, splicePoint + 1); // Restore selection
    };

    let {value, selectionStart} = textarea;
    let updatedContent = spliceContent(value, selectionStart)

    updateTextarea(textarea, updatedContent, selectionStart);
  };

  /------------------------------------------------------------------------------/

  // Insert or remove a 'prefix' string from the current cursor position in the 'textarea.
  function togglePrefix (textarea, prefix) {
    textarea.focus();

    if (!prefix) {
      throw new Error('The "prefix" parameter is required.');
    }

    const expandSelectionToFullLines = (content, selectionStart, selectionEnd) => {
      while (selectionStart > 0 && content[selectionStart - 1] !== '\\n') {
        selectionStart--;
      }
      while (selectionEnd < content.length && content[selectionEnd] !== '\\n') {
        selectionEnd++;
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
    const adjustedStartPos = textarea.selectionStart + prefix.length;
    const adjustedEndPos = selectionStart + updatedLines.join('\\n').length;

    updateTextarea(textarea, updatedContent, adjustedStartPos, adjustedEndPos);
  };

  /------------------------------------------------------------------------------/

  // Surround the selected section of the 'textarea' with the 'wrapper' string.
  function surroundFocus (textarea, wrapper) {
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
  
    // Remove any instances of the `wrapper` string from  inside the selection
    // but otherwise preserve the inner content.
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
  };

  /------------------------------------------------------------------------------/

  function validateField(field) {
     this.fields[field].isValid = this.fields[field].value.trim() !== '';
  };

  function allValid() {
    return Object.values(this.fields).every(field => field.isValid && field.value.trim() !== '');
  };

  /------------------------------------------------------------------------------/

  async function loadPreview() {
    this.editMode = false;

    // TODO: Figure out how to set proper hostname here:
    const response = await fetch('http://localhost:2000/markdown', {
      method: 'POST',
      headers: {
        'Content-Type': 'text/plain;charset=utf-8'
      },
      body: this.fields.body.value
      });

    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }

    const data = await response.text();

    this.fields.body.valueParsed = data;
  };

  /------------------------------------------------------------------------------/

  function alpineHandler() {
    return {
      file: null,
      fileName: '',
      selectAndUpload() {
        // Open the file picker
        this.$refs.fileInput.click(); 
      },
      uploadFile,
      togglePrefix,
      surroundFocus,
      validateField,
      allValid,
      loadPreview,
    };
  }
|]
