module App.Context.Otel
  ( -- * OTel Context
    OtelContext (..),
    HasOtelContext (..),

    -- * Re-exports
    OTEL.Tracer,
    OTEL.TracerProvider,
  )
where

--------------------------------------------------------------------------------

import Data.Has qualified as Has
import OpenTelemetry.Trace qualified as OTEL

--------------------------------------------------------------------------------

-- | Context type containing OpenTelemetry resources.
-- Users can compose this with their custom context type.
data OtelContext = OtelContext
  { otelTracer :: OTEL.Tracer,
    otelTracerProvider :: OTEL.TracerProvider
  }

instance Has.Has OTEL.Tracer OtelContext where
  getter = otelTracer
  modifier f ctx@OtelContext {otelTracer} = ctx {otelTracer = f otelTracer}

instance Has.Has OTEL.TracerProvider OtelContext where
  getter = otelTracerProvider
  modifier f ctx@OtelContext {otelTracerProvider} = ctx {otelTracerProvider = f otelTracerProvider}

-- | Typeclass for contexts that contain an OtelContext.
-- This allows the OTel-enabled functions to work with any context
-- that has OTel resources embedded in it.
class HasOtelContext ctx where
  getOtelContext :: ctx -> OtelContext

instance HasOtelContext OtelContext where
  getOtelContext = id
