module API.Markdown.Post where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Errors (InternalServerError (InternalServerError), Unauthorized (..), throwErr)
import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (unless, (>=>))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Text.Encoding as TE
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
  "markdown"
    :> Servant.ReqBody '[Servant.PlainText] Text
    :> Servant.Post '[HTML] RawHtml

--------------------------------------------------------------------------------

handler ::
  ( Applicative m,
    Has Trace.Tracer env,
    MonadCatch m,
    Log.MonadLog m,
    MonadUnliftIO m,
    MonadReader env m
  ) =>
  Text ->
  m RawHtml
handler markdown = do
  Observability.handlerSpan "GET /markdown" markdown display $ do
    nodes <- processInput markdown
    pure $ renderNodes nodes

processInput ::
  ( MonadIO m,
    Log.MonadLog m,
    MonadThrow m
  ) =>
  Text ->
  m [Xml.Node]
processInput =
  parseMarkdown >=> (TE.encodeUtf8 >>> parseFragment) >>> fmap applyStyle

parseMarkdown :: (MonadIO m, Log.MonadLog m, MonadThrow m) => Text -> m Text
parseMarkdown md = do
  Log.logTrace "Pandoc Markdown Input" md
  pandocResult <- liftIO . runIO $ do
    let extensions = [Ext_backtick_code_blocks, Ext_hard_line_breaks, Ext_task_lists]
        readerConfig = def {readerExtensions = extensionsFromList extensions}
        writerConfig = def {writerExtensions = extensionsFromList extensions}
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
    & set (traversed . _el "ol" . _elAttributes) [("class", "text-gray-900 list-decimal list-inside ml-1")]
    & set (traversed . _el "ul" . _elAttributes) [("class", "text-gray-900 list-disc list-inside ml-1")]
    & over (traversed . _el "ul" . _Node . _el "li" . _Node . _el "label" . _Node . _el "input" . _elAttributes) (<> [("class", "mr-1")])
    & set (traversed . _el "h1" . _elAttributes) [("class", "text-5xl font-extrabold")]
    & set (traversed . _el "h2" . _elAttributes) [("class", "text-4xl font-extrabold")]
    & set (traversed . _el "h3" . _elAttributes) [("class", "text-3xl font-extrabold")]
    & set (traversed . _el "h4" . _elAttributes) [("class", "text-2xl font-extrabold")]
    & set (traversed . _el "h5" . _elAttributes) [("class", "text-xl font-extrabold")]
    & set (traversed . _el "h6" . _elAttributes) [("class", "text-lg font-extrabold")]
    & set (traversed . _el "blockquote" . _elAttributes) [("class", "border-l-4 px-2 text-gray-600")]
    & set (traversed . _el "p" . _Node . _el "code" . _elAttributes) [("class", "font-mono rounded-md p-0.5 bg-gray-200")]
    & set (traversed . _el "pre" . _elAttributes) [("class", "font-mono rounded-md p-2 bg-gray-200")]
    & over (traversed . _el "a" . _elAttributes) (<> [("class", "text-blue-600 hover:underline")])
