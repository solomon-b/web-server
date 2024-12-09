{-# LANGUAGE QuasiQuotes #-}

module API.Blog.Id.Preview.Get where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Errors (Unauthorized (..), throwErr)
import CMark qualified
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.ByteString (ByteString)
import Data.Foldable (fold)
import Data.Has (Has)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
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
    :> Servant.Capture "id" BlogPosts.Id
    :> "preview"
    :> Servant.QueryParam "content" Text
    :> Servant.Get '[HTML] RawHtml

--------------------------------------------------------------------------------

emptyPreview :: Text
emptyPreview = "<p name='content' rows='8' class='p-2 w-full text-sm text-gray-900'>Nothing to preview</p>"

template :: BlogPosts.Id -> Maybe Text -> ByteString
template bid content =
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
          <button role='tab' hx-get="/blog/#{display bid}/preview" hx-swap="innerHTML" hx-target="\#content-field" hx-include='{"content": "#{fromMaybe emptyPreview content}"}'>
            Preview
          </button>
        </div>

        <div class='p-2 border-b rounded-t-lg border-gray-300 text-gray-500 bg-gray-50 grow flex justify-end'>
        </div>
    </div>
    <div class='m-2 min-h-60'>#{fromMaybe emptyPreview content}</div>
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
  BlogPosts.Id ->
  Maybe Text ->
  m RawHtml
handler (Auth.Authz User.Domain {..} _) bid content =
  Observability.handlerSpan "GET /blog/new/preview" () display $ do
    unless dIsAdmin $ throwErr Unauthorized
    pageFragment <- parseFragment $ template bid $ fmap (CMark.commonmarkToHtml []) content
    pure $ renderNodes pageFragment
