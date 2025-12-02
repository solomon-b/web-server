{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Context
  ( -- * Re-exports from web-server-core
    AppContext (..),
    -- * Example-specific custom context
    ExampleCustomCtx (..),
    ServantContext,
  )
where

--------------------------------------------------------------------------------

import App.Auth (Authz)
import App.Config (SmtpConfig)
import Data.Has qualified as Has
import Network.Wai qualified as Wai
import Servant qualified
import Servant.Server.Experimental.Auth (AuthHandler)

-- Re-export AppContext from web-server-core
import "web-server-core" App.Context (AppContext (..))

--------------------------------------------------------------------------------
-- Example-specific custom context

newtype ExampleCustomCtx = ExampleCustomCtx
  { exampleSmtpConfig :: Maybe SmtpConfig
  }

instance Has.Has (Maybe SmtpConfig) ExampleCustomCtx where
  getter = exampleSmtpConfig
  modifier f ctx = ctx {exampleSmtpConfig = f (exampleSmtpConfig ctx)}

-- Forward Has lookups from AppContext to the custom context
-- This allows MonadEmail and other effects to access app-specific config
instance (Has.Has (Maybe SmtpConfig) ctx) => Has.Has (Maybe SmtpConfig) (AppContext ctx) where
  getter = Has.getter . appCustom
  modifier f ctx@AppContext {appCustom} = ctx {appCustom = Has.modifier f appCustom}

-- | Example's ServantContext includes ErrorFormatters
type ServantContext = '[Servant.ErrorFormatters, AuthHandler Wai.Request Authz]
