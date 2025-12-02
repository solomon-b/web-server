{-# LANGUAGE PackageImports #-}

module App (runApp) where

--------------------------------------------------------------------------------

import API (API, server)
import App.Config (SmtpConfigF)
import App.Config.Fetchers (toConcrete, fromEnv)
import App.Context (ExampleCustomCtx (..))
import "web-server-core" App qualified as Core

--------------------------------------------------------------------------------

-- | Fetch example-specific config (SMTP) and run the app using web-server-core's runApp.
-- The core config is fetched internally by Core.runApp.
runApp :: IO ()
runApp = do
  smtpConfig <- toConcrete (fromEnv @SmtpConfigF)
  let customCtx = ExampleCustomCtx smtpConfig
  Core.runApp @API server customCtx
