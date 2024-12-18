module API.Blog.New.Preview.Get where

--------------------------------------------------------------------------------

import API.Blog.New.Template
import App.Auth qualified as Auth
import App.Errors (InternalServerError (InternalServerError), Unauthorized (..), throwErr)
import Control.Lens
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Traversable (forM)
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import Log qualified
import OpenTelemetry.Trace qualified as Trace
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML, RawHtml, parseFragment, renderNodes)
import Text.Pandoc
import Text.XmlHtml qualified as Xml
import Text.XmlHtml.Optics

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> "blog"
    :> "new"
    :> "preview"
    :> Servant.QueryParam "content" Text
    :> Servant.Get '[HTML] RawHtml

--------------------------------------------------------------------------------

handler ::
  ( Applicative m,
    Has Trace.Tracer env,
    MonadCatch m,
    Log.MonadLog m,
    MonadUnliftIO m,
    MonadReader env m
  ) =>
  Auth.Authz ->
  Maybe Text ->
  m RawHtml
handler (Auth.Authz User.Domain {..} _) content = do
  Observability.handlerSpan "GET /blog/new/preview" () display $ do
    unless dIsAdmin $ throwErr Unauthorized
    contentHtml <- forM content parseMarkdown
    pageFragment <- parseFragment $ contentFieldPreview contentHtml
    let pageFragment' = applyStyle pageFragment
    pure $ renderNodes pageFragment'

parseMarkdown :: (MonadIO m, Log.MonadLog m, MonadThrow m) => Text -> m Text
parseMarkdown md = do
  Log.logTrace "Pandoc Markdown Input" md
  pandocResult <- liftIO . runIO $ do
    let readerConfig = def { readerExtensions = extensionsFromList [Ext_fenced_code_blocks, Ext_hard_line_breaks] }
        writerConfig = def { writerExtensions =  extensionsFromList [Ext_fenced_code_blocks, Ext_hard_line_breaks] }
    pandocAst <- readMarkdown readerConfig md
    result <- writeHtml5String writerConfig pandocAst
    pure (pandocAst, result)
  (ast, html) <- handleMarkdownError pandocResult
  Log.logTrace "Pandoc AST" ast
  Log.logTrace "Pandoc HTML Output" html
  pure html

handleMarkdownError :: (Log.MonadLog m, MonadThrow m) => Either PandocError (Pandoc, Text) -> m (Pandoc, Text)
handleMarkdownError = \case
  Left err -> do
    let err' = Text.pack $ show err
    throwErr $ InternalServerError err'
  Right (ast, html) -> pure (ast, html)

applyStyle :: [Xml.Node] -> [Xml.Node]
applyStyle nodes =
  nodes
    & set (traversed . _el "p" . _elAttributes) [("class", "my-3")]
    & set (traversed . _el "ol" . _elAttributes) [("class", "max-w-md space-y-1 text-gray-900 list-decimal list-inside")]
    & set (traversed . _el "ul" . _elAttributes) [("class", "max-w-md space-y-1 text-gray-900 list-disc list-inside")]
    & set (traversed . _el "h1" . _elAttributes) [("class", "text-5xl font-extrabold")]
    & set (traversed . _el "h2" . _elAttributes) [("class", "text-4xl font-extrabold")]
    & set (traversed . _el "h3" . _elAttributes) [("class", "text-3xl font-extrabold")]
    & set (traversed . _el "h4" . _elAttributes) [("class", "text-2xl font-extrabold")]
    & set (traversed . _el "h5" . _elAttributes) [("class", "text-xl font-extrabold")]
    & set (traversed . _el "h6" . _elAttributes) [("class", "text-lg font-extrabold")]
    & set (traversed . _el "blockquote" . _elAttributes) [("class", "text-xl italic font-semibold text-gray-900")]
    & set (traversed . _el "code" . _elAttributes) [("class", "font-mono rounded-md p-0.5 bg-gray-200")]
