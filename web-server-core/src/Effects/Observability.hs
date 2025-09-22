{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Effects.Observability where

--------------------------------------------------------------------------------

import App.Config (AppExporter (..), ObservabilityConfig (..), Verbosity (..))
import Control.Exception (bracket)
import Control.Monad.Catch (MonadCatch, MonadThrow (..), catchAll)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.IO.Unlift
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader qualified as Reader
import Data.Data (Proxy (..))
import Data.Fixed (Pico)
import Data.Has qualified as Has
import Data.HashMap.Strict qualified as HashMap
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display, display)
import Data.Text.Lazy qualified as Lazy
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX qualified as Time
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.Wai (Request (..))
import OpenTelemetry.Attributes (getAttributes)
import OpenTelemetry.Exporter.Handle.Span (stdoutExporter')
import OpenTelemetry.Exporter.Span (ExportResult (..), SpanExporter (..))
import OpenTelemetry.Processor.Simple (SimpleProcessorConfig (SimpleProcessorConfig), simpleProcessor)
import OpenTelemetry.Trace (ImmutableSpan (..))
import OpenTelemetry.Trace qualified as OTEL
import OpenTelemetry.Trace.Core qualified as Trace
import OpenTelemetry.Util (appendOnlyBoundedCollectionValues)
import Servant qualified
import Servant.Server.Internal.Delayed (Delayed (..), addMethodCheck)
import Servant.Server.Internal.DelayedIO (withRequest)
import Servant.Server.Internal.Router (Router)
import qualified Servant.API as Links

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
        Otel -> do
          putStrLn "Using StdOut Exporter"
          OTEL.initializeGlobalTracerProvider
        StdOut -> do
          putStrLn "Using StdOut Exporter"
          mkAcquire $ stdoutExporter' (pure . stdoutFormatter verbosity)
        None -> do
          putStrLn "Open Telemetry Exporter Disabled"
          mkAcquire noOpExporter
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
    MonadCatch m,
    MonadUnliftIO m
    -- Log.MonadLog m,
  ) =>
  Text ->
  m a ->
  m a
handlerSpan handlerName handlerAction = do
  -- TODO: Disambiguate Response data from rendered Response
  tracer <- Reader.asks Has.getter
  OTEL.inSpan' tracer ("handler " <> handlerName) OTEL.defaultSpanArguments $ \reqSpan -> do
    -- OTEL.addEvent reqSpan $
    --   OTEL.NewEvent
    --     { newEventName = "handler request",
    --       newEventAttributes = HashMap.fromList [("request", OTEL.toAttribute . display $ req)],
    --       newEventTimestamp = Nothing
    --     }

    handlerResult <-
      handlerAction `catchAll` \exception -> do
        OTEL.addEvent reqSpan $
          OTEL.NewEvent
            { newEventName = "handler error",
              newEventAttributes = HashMap.fromList [("error", OTEL.toAttribute . Text.pack . show $ exception)],
              newEventTimestamp = Nothing
            }
        throwM exception

    -- OTEL.addEvent reqSpan $
    --   OTEL.NewEvent
    --     { newEventName = "handler success",
    --       newEventAttributes = HashMap.fromList [("response", OTEL.toAttribute . display . getRes $ handlerResult)],
    --       newEventTimestamp = Nothing
    --     }

    -- TODO: Disambiguate Response data from rendered Response
    -- Log.logInfo handlerName $ Aeson.object ["request-data" .= req, "response-data" .= ("getRes handlerResult" :: Text)]
    pure handlerResult

--------------------------------------------------------------------------------

newtype Print api = Print api

instance (Servant.HasServer api ctx) => Servant.HasServer (Print api) ctx where
  type ServerT (Print api) m = Servant.ServerT api m

  route ::
    Proxy (Print api) ->
    Servant.Context ctx ->
    Delayed env (Servant.ServerT api Servant.Handler) ->
    Router env
  route _ context delayed =
    Servant.route (Proxy :: Proxy api) context $
      addMethodCheck delayed $
        withRequest (\_req -> liftIO $ putStrLn "HELLO WORLD !!!!!!!!!!!!!!!!!!!!")

  hoistServerWithContext _ = Servant.hoistServerWithContext (Proxy :: Proxy api)

--------------------------------------------------------------------------------

data WithSpan (label :: Symbol) api

instance
  ( KnownSymbol label,
    Servant.HasServer api context,
    Servant.HasContextEntry context OTEL.Tracer
  ) =>
  Servant.HasServer (WithSpan label api) context
  where
  type ServerT (WithSpan label api) m = OTEL.Tracer -> Servant.ServerT api m

  route _ ctx delayed =
    let tracer :: OTEL.Tracer = Servant.getContextEntry ctx
        handlerName = "Handler: " <> Text.pack (symbolVal (Proxy @label))
     in Servant.route (Proxy @api) ctx $
          addMethodCheck (fmap ($ tracer) delayed) $
            withRequest $ \req ->
              liftIO $
                OTEL.inSpan' tracer handlerName OTEL.defaultSpanArguments $ \reqSpan -> do
                  OTEL.addEvent reqSpan $
                    OTEL.NewEvent
                      { newEventName = "handler request",
                        newEventAttributes = HashMap.fromList [("request", OTEL.toAttribute . Text.pack $ show req)],
                        newEventTimestamp = Nothing
                      }

  hoistServerWithContext _ pc nt handler tracer = Servant.hoistServerWithContext (Proxy :: Proxy api) pc nt (handler tracer)

instance forall label sub. (Links.HasLink sub) => Links.HasLink (WithSpan label sub) where
  type MkLink (WithSpan label sub) a = Links.MkLink sub a
  toLink toA _ = Links.toLink toA (Proxy @sub)

wrapInSpan :: (Display a) => OTEL.Tracer -> Text -> Request -> Servant.Handler a -> Servant.Handler a
wrapInSpan tracer handlerName req (Servant.Handler (ExceptT action)) =
  Servant.Handler . ExceptT $
    withRunInIO $ \runInIO ->
      OTEL.inSpan' tracer handlerName OTEL.defaultSpanArguments $ \reqSpan -> do
        OTEL.addEvent reqSpan $
          OTEL.NewEvent
            { newEventName = "handler request",
              newEventAttributes = HashMap.fromList [("request", OTEL.toAttribute . Text.pack $ show req)],
              newEventTimestamp = Nothing
            }

        response <- runInIO action

        case response of
          Left err -> do
            OTEL.addEvent reqSpan $
              OTEL.NewEvent
                { newEventName = "handler error",
                  newEventAttributes = HashMap.fromList [("error", OTEL.toAttribute . Text.pack $ show err)],
                  newEventTimestamp = Nothing
                }
            pure response
          Right success -> do
            OTEL.addEvent reqSpan $
              OTEL.NewEvent
                { newEventName = "handler result",
                  newEventAttributes =
                    HashMap.fromList
                      [("result", OTEL.toAttribute (display success))],
                  newEventTimestamp = Nothing
                }
            pure response
