{-# LANGUAGE PackageImports #-}

module App (runApp) where

import API (API, server)
import App.Config.Fetchers (FetchHKD (..))
import App.Otel
  ( AppExporter (..),
    HasOtelContext (..),
    ObservabilityConfig (..),
    ObservabilityConfigF,
    OtelContext,
    runServerWithOtel,
    withOtelResources,
  )
import "web-server-core" App qualified as Core
import "web-server-core" App.Context (AppContext (..))
import Data.Maybe (fromMaybe)

--------------------------------------------------------------------------------

-- | Custom context for the example app.
-- Contains OTel configuration and initialized resources.
newtype ExampleContext = ExampleContext { exampleOtel :: OtelContext }
instance HasOtelContext ExampleContext where
  getOtelContext = exampleOtel

--------------------------------------------------------------------------------

-- | Run the example app with OpenTelemetry tracing enabled.
runApp :: IO ()
runApp = do
  -- Fetch OTel config from environment before initializing app resources
  mOtelConfig <- toConcrete (fromEnv @ObservabilityConfigF)
  let otelConfig = fromMaybe (ObservabilityConfig None) mOtelConfig

  Core.withAppResources otelConfig $ \coreCtx -> do
    let verbosity = appVerbosity coreCtx
        obsConfig = appCustom coreCtx

    withOtelResources (appLoggerEnv coreCtx) verbosity obsConfig $ \otelCtx -> do
      let exampleCtx = coreCtx {appCustom = ExampleContext otelCtx}
      runServerWithOtel @API server exampleCtx
