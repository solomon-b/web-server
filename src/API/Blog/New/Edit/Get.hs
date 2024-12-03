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
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
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
    :> Servant.Get '[HTML] RawHtml

--------------------------------------------------------------------------------

template :: ByteString
template =
  [i|
                            <label for='content' class='mb-2 text-sm text-gray-900 font-semibold'>Add body</label>
                            <div class='flex flex-col border border-gray-300 rounded-lg'>
                                <div class='flex mb-2'>
                                    <div class='p-2 border-r border-gray-300 rounded-t-lg bg-white text-gray-900'>
                                      <button href='\#' role='tab' hx-get="/blog/new/edit" hx-swap="innerHTML" hx-target="\#content-field">
                                        Write
                                      </button>
                                    </div>

                                    <div class='p-2 border-b border-gray-300 rounded-t-lg bg-gray-50 text-gray-500'>
                                      <button href='\#' role='tab' hx-get="/blog/new/preview" hx-swap="innerHTML" hx-target="\#content-field">
                                        Preview
                                      </button>
                                    </div>

                                    <div class='p-2 border-b border-gray-300 rounded-t-lg bg-gray-50 text-gray-500 grow flex justify-end'>
                                        <i title="Heading" class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-heading'></i>
                                        <i title="Bold" class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-bold'></i>
                                        <i title="Italic" class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-italic'></i>
                                        <i class="border-l border-t-0 border-gray-300"></i>
                                        <i title="Quote" class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-quote-left'></i>
                                        <i title="Code" class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-code'></i>
                                        <i title="Link" class='p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-link'></i>
                                        <i class="border-l border-t-0 border-gray-300"></i>
                                        <i title="Numbered List" class="p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-list-ol"></i>
                                        <i title="Unordered List" class="p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-list"></i>
                                        <i title="Task List" class="p-1 px-2 rounded-lg hover:bg-gray-200 fa-solid fa-list-check"></i>
                                    </div>
                                </div>
                                <div class='m-2 min-h-60'>
                                    <textarea name='content' placeholder='Add your content here...' rows='11' class='block p-2.5 w-full text-sm text-gray-900 bg-gray-50 rounded-lg border border-gray-300 focus:ring-blue-500 focus:border-blue-500'></textarea>
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
  m RawHtml
handler (Auth.Authz User.Domain {..} _) =
  Observability.handlerSpan "GET /blog/new/edit" () (const @Text "RawHtml") $ do
    unless dIsAdmin $ throwErr Unauthorized
    pageFragment <- parseFragment template
    pure $ renderNodes pageFragment
