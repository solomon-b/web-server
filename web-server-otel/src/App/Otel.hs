{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App.Otel
  ( -- * OTel-enabled server
    runServerWithOtelContext,

    -- * Resource management
    withOtelResources,

    -- * Re-exports
    module App.Context.Otel,
    module Config.Otel,
    module Effects.Observability,
  )
where

--------------------------------------------------------------------------------

import App (interpret, mkWarpSettings)
import App.Config (Verbosity)
import App.Context (AppContext (..))
import App.Context.Otel
import App.Monad (AppM)
import Config.Otel
import Control.Monad.Reader qualified as Reader
import Data.Data (Proxy (..))
import Effects.Observability
import Log qualified
import Network.Wai.Handler.Warp qualified as Warp
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware')
import OpenTelemetry.Trace qualified as OTEL
import OpenTelemetry.Trace.Monad (MonadTracer (..))
import Servant (Context ((:.)))
import Servant qualified

--------------------------------------------------------------------------------

-- | Initialize OTel resources in a bracket pattern.
-- Provides an OtelContext that can be composed with user's custom context.
withOtelResources ::
  Log.LoggerEnv ->
  Verbosity ->
  ObservabilityConfig ->
  (OtelContext -> IO a) ->
  IO a
withOtelResources logEnv verbosity otelConfig action =
  withTracer logEnv verbosity otelConfig $ \tracerProvider mkTracer -> do
    let tracer = mkTracer OTEL.tracerOptions
        otelCtx =
          OtelContext
            { otelTracer = tracer,
              otelTracerProvider = tracerProvider
            }
    action otelCtx

-- | Run an OpenTelemetry-traced HTTP server. The caller supplies the Servant
-- auth 'Context' (its own auth handlers); the OTel tracer is prepended
-- automatically, so the served context is @OTEL.Tracer ': authCtx@. The context
-- must reach 'OtelContext' via 'HasOtelContext' (typically stored in the app's
-- custom context). This is the sole server entry point. The app owns the auth
-- principal, so plain and role-gated routes can share one handler.
runServerWithOtelContext ::
  forall api authCtx ctx.
  ( Servant.HasServer api (OTEL.Tracer ': authCtx),
    Servant.HasContextEntry ((OTEL.Tracer ': authCtx) Servant..++ Servant.DefaultErrorFormatters) Servant.ErrorFormatters,
    HasOtelContext ctx
  ) =>
  Servant.Context authCtx ->
  (AppContext ctx -> Servant.ServerT api (AppM ctx)) ->
  AppContext ctx ->
  IO ()
runServerWithOtelContext callerCtx server appCtx = do
  let otelCtx = getOtelContext (appCustom appCtx)
      otelMiddleware = newOpenTelemetryWaiMiddleware' (otelTracerProvider otelCtx)
      fullCtx = otelTracer otelCtx :. callerCtx
      warpSettings = mkWarpSettings (appLoggerEnv appCtx) (appWarpConfig appCtx)
  Warp.runSettings warpSettings (otelMiddleware $ mkAppWithOtelContext @api @authCtx server fullCtx appCtx)

-- | Create a Servant application with the OTel tracer prepended to a
-- caller-supplied auth context. Hoists the handler monad with the full
-- @OTEL.Tracer ': authCtx@ context.
mkAppWithOtelContext ::
  forall api authCtx ctx.
  ( Servant.HasServer api (OTEL.Tracer ': authCtx),
    Servant.HasContextEntry ((OTEL.Tracer ': authCtx) Servant..++ Servant.DefaultErrorFormatters) Servant.ErrorFormatters
  ) =>
  (AppContext ctx -> Servant.ServerT api (AppM ctx)) ->
  Context (OTEL.Tracer ': authCtx) ->
  AppContext ctx ->
  Servant.Application
mkAppWithOtelContext server fullCtx appCtx =
  Servant.serveWithContext (Proxy @api) fullCtx $
    Servant.hoistServerWithContext
      (Proxy @api)
      (Proxy @(OTEL.Tracer ': authCtx))
      (interpret appCtx)
      (server appCtx)

--------------------------------------------------------------------------------

-- | MonadTracer instance for AppM when the custom context has OTel resources.
-- This is an orphan instance since AppM is defined in web-server-core.
instance (HasOtelContext ctx) => MonadTracer (AppM ctx) where
  getTracer = Reader.asks (otelTracer . getOtelContext . appCustom)
