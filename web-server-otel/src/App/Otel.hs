{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App.Otel
  ( -- * OTel-enabled server
    runServerWithOtel,
    OtelServerContext,

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
import App.Auth (Authz, authHandler)
import App.Config (Verbosity)
import App.Context (AppContext (..))
import App.Context.Otel
import App.Monad (AppM)
import Control.Monad.Reader qualified as Reader
import Log qualified
import OpenTelemetry.Trace.Monad (MonadTracer (..))
import Config.Otel
import Data.Data (Proxy (..))
import Effects.Observability
import Network.Wai (Request)
import Network.Wai.Handler.Warp qualified as Warp
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware')
import OpenTelemetry.Trace qualified as OTEL
import Servant (Context ((:.)))
import Servant qualified
import Servant.Server.Experimental.Auth (AuthHandler)

--------------------------------------------------------------------------------

-- | Server context for OTel-enabled servers.
-- Includes the tracer in the Servant context for use with WithSpan combinator.
type OtelServerContext = '[OTEL.Tracer, AuthHandler Request Authz]

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

-- | Run the HTTP server with OpenTelemetry tracing enabled.
-- This adds OTel middleware and includes the tracer in the Servant context.
--
-- The context type must provide access to OtelContext via HasOtelContext.
-- Typically users will store OtelContext in their custom context:
--
-- @
-- data MyContext = MyContext
--   { myOtel :: OtelContext
--   , myOtherStuff :: ...
--   }
--
-- instance HasOtelContext MyContext where
--   getOtelContext = myOtel
-- @
runServerWithOtel ::
  forall api ctx.
  ( Servant.HasServer api OtelServerContext,
    HasOtelContext ctx
  ) =>
  (AppContext ctx -> Servant.ServerT api (AppM ctx)) ->
  AppContext ctx ->
  IO ()
runServerWithOtel server appCtx = do
  let otelCtx = getOtelContext (appCustom appCtx)
      otelMiddleware = newOpenTelemetryWaiMiddleware' (otelTracerProvider otelCtx)
      servantContext = otelTracer otelCtx :. authHandler (appDbPool appCtx) :. Servant.EmptyContext
      warpSettings = mkWarpSettings (appLoggerEnv appCtx) (appWarpConfig appCtx)
  Warp.runSettings warpSettings (otelMiddleware $ mkAppWithOtel @api server servantContext appCtx)

-- | Create a Servant application with OTel context.
mkAppWithOtel ::
  forall api ctx.
  (Servant.HasServer api OtelServerContext) =>
  (AppContext ctx -> Servant.ServerT api (AppM ctx)) ->
  Context OtelServerContext ->
  AppContext ctx ->
  Servant.Application
mkAppWithOtel server fullCtx appCtx =
  Servant.serveWithContext (Proxy @api) fullCtx $
    Servant.hoistServerWithContext
      (Proxy @api)
      (Proxy @OtelServerContext)
      (interpret appCtx)
      (server appCtx)

--------------------------------------------------------------------------------

-- | MonadTracer instance for AppM when the custom context has OTel resources.
-- This is an orphan instance since AppM is defined in web-server-core.
instance (HasOtelContext ctx) => MonadTracer (AppM ctx) where
  getTracer = Reader.asks (otelTracer . getOtelContext . appCustom)

