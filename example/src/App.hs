{-# LANGUAGE PackageImports #-}

module App (runApp) where

import API (API, server)
import "web-server-core" App qualified as Core

-- | Run the minimal example app using web-server-core's runApp.
-- Uses () as custom context since no SMTP or other example-specific config is needed.
runApp :: IO ()
runApp = Core.runApp @API server ()
