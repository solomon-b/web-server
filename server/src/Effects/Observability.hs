module Effects.Observability where

--------------------------------------------------------------------------------

import App.Config (AppExporter (..), ObservabilityConfig (..), Verbosity (..))
import Control.Exception (bracket)
import Control.Monad.Catch (MonadCatch, MonadThrow (..), catchAll)
import Control.Monad.IO.Unlift
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader qualified as Reader
import Data.Fixed (Pico)
import Data.Has qualified as Has
import Data.HashMap.Strict qualified as HashMap
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display
import Data.Text.Lazy qualified as Lazy
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX qualified as Time
import Data.Time.Format.ISO8601 (iso8601Show)
import OpenTelemetry.Attributes (getAttributes)
import OpenTelemetry.Exporter.Handle.Span (stdoutExporter')
import OpenTelemetry.Exporter.Span (ExportResult (..), SpanExporter (..))
import OpenTelemetry.Processor.Simple (SimpleProcessorConfig (SimpleProcessorConfig), simpleProcessor)
import OpenTelemetry.Trace (ImmutableSpan (..))
import OpenTelemetry.Trace qualified as OTEL
import OpenTelemetry.Trace.Core qualified as Trace
import OpenTelemetry.Util (appendOnlyBoundedCollectionValues)

--------------------------------------------------------------------------------

printPrimitiveAttribute :: Trace.PrimitiveAttribute -> Lazy.Text
printPrimitiveAttribute (Trace.TextAttribute t) = Lazy.fromStrict t
printPrimitiveAttribute (Trace.BoolAttribute b) = Lazy.pack . show $ b
printPrimitiveAttribute (Trace.DoubleAttribute d) = Lazy.pack . show $ d
printPrimitiveAttribute (Trace.IntAttribute i) = Lazy.pack . show $ i

printAttribute :: Trace.Attribute -> Lazy.Text
printAttribute (Trace.AttributeValue p) = printPrimitiveAttribute p
printAttribute (Trace.AttributeArray a) = "[ " <> Lazy.intercalate " ," (fmap printPrimitiveAttribute a) <> " ]"

stdoutFormatter :: Verbosity -> ImmutableSpan -> Lazy.Text
stdoutFormatter verbosity ImmutableSpan {..} =
  let start =
        Lazy.pack . iso8601Show . Time.posixSecondsToUTCTime . secondsToNominalDiffTime $
          (fromIntegral (Trace.timestampNanoseconds spanStart) :: Pico) / 1000000000
      mkDuration =
        Lazy.pack
          . (<> "ms")
          . show
          . (/ (1000000 :: Double))
          . fromIntegral
          . (Trace.timestampNanoseconds spanStart `subtract`)
          . Trace.timestampNanoseconds
      duration = maybe "" mkDuration spanEnd
      name = Lazy.fromStrict spanName
      -- attrs = foldMapWithKey (\k v -> L.pack (show k) <> ": " <> printAttribute v <> "\n") (snd $ getAttributes spanAttributes)
      spanIdText =
        Lazy.pack (show $ Trace.traceId spanContext)
          <> ":"
          <> (Lazy.pack . show $ Trace.spanId spanContext)
      events =
        Lazy.fromStrict
          ( foldMap
              ( \Trace.Event {..} ->
                  let ts =
                        Lazy.pack . iso8601Show . Time.posixSecondsToUTCTime . secondsToNominalDiffTime $
                          (fromIntegral (Trace.timestampNanoseconds spanStart) :: Pico) / 1000000000
                      sortedEventList = sortOn fst . HashMap.toList . snd . getAttributes $ eventAttributes
                      eventAttrs =
                        foldMap
                          (\(k, v) -> Lazy.pack (show k) <> ": " <> printAttribute v <> "\n")
                          sortedEventList
                   in "[EVENT: " <> eventName <> "] - " <> Lazy.toStrict ts <> "\n" <> Lazy.toStrict eventAttrs
              )
              (appendOnlyBoundedCollectionValues spanEvents)
          )
      additionalInformation = case verbosity of
        Quiet -> ""
        _ -> events
   in "[" <> spanIdText <> "]" <> " " <> name <> " - " <> start <> " " <> duration <> "\n" <> additionalInformation

noOpExporter :: SpanExporter
noOpExporter = SpanExporter (\_ -> pure Success) (pure ())

mkAcquire :: SpanExporter -> IO Trace.TracerProvider
mkAcquire exporter = do
  providerOpts <- snd <$> OTEL.getTracerProviderInitializationOptions
  processor <- simpleProcessor . SimpleProcessorConfig $ exporter
  OTEL.createTracerProvider [processor] providerOpts

withTracer :: ObservabilityConfig -> (OTEL.TracerProvider -> (OTEL.TracerOptions -> OTEL.Tracer) -> IO c) -> IO c
withTracer (ObservabilityConfig verbosity exporter) f =
  let acquire = case exporter of
        Otel -> OTEL.initializeGlobalTracerProvider
        StdOut -> mkAcquire $ stdoutExporter' (pure . stdoutFormatter verbosity)
        None -> mkAcquire noOpExporter
      release = case exporter of
        None -> \_ -> pure ()
        _ -> OTEL.shutdownTracerProvider
      -- TODO: Propagate hostname here:
      work tracerProvider = f tracerProvider $ OTEL.makeTracer tracerProvider "web-server"
   in bracket acquire release work

handlerSpan ::
  ( MonadReader env m,
    Has.Has OTEL.Tracer env,
    MonadIO m,
    Display req,
    MonadCatch m,
    MonadUnliftIO m,
    Display res
  ) =>
  Text ->
  req ->
  (a -> res) ->
  m a ->
  m a
handlerSpan handlerName req getRes handlerAction = do
  tracer <- Reader.asks Has.getter
  OTEL.inSpan' tracer ("handler " <> handlerName) OTEL.defaultSpanArguments $ \reqSpan -> do
    OTEL.addEvent reqSpan $
      OTEL.NewEvent
        { newEventName = "handler request",
          newEventAttributes = HashMap.fromList [("request", OTEL.toAttribute . display $ req)],
          newEventTimestamp = Nothing
        }

    handlerResult <-
      handlerAction `catchAll` \exception -> do
        OTEL.addEvent reqSpan $
          OTEL.NewEvent
            { newEventName = "handler error",
              newEventAttributes = HashMap.fromList [("error", OTEL.toAttribute . Text.pack . show $ exception)],
              newEventTimestamp = Nothing
            }
        throwM exception

    OTEL.addEvent reqSpan $
      OTEL.NewEvent
        { newEventName = "handler success",
          newEventAttributes = HashMap.fromList [("response", OTEL.toAttribute . display . getRes $ handlerResult)],
          newEventTimestamp = Nothing
        }

    pure handlerResult
