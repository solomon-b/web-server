{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deriving-defaults #-}

module App.Config
  ( -- * Re-exports from web-server-core
    module Core,
    -- * SMTP Config (example-specific)
    SmtpConfig (..),
    SmtpConfigF (..),
    -- * Extended App Config with SMTP
    ExampleAppConfig (..),
    getExampleConfig,
  )
where

--------------------------------------------------------------------------------

import App.Config.Fetchers (FetchHKD (..), parseEnvStr)
import Barbies
import Data.Functor.Classes (Show1)
import Data.Functor.Compose (Compose (..))
import Data.Text (Text)
import GHC.Generics

-- Re-export everything from web-server-core's App.Config
import "web-server-core" App.Config as Core

--------------------------------------------------------------------------------
-- SMTP Config (example-specific)

data SmtpConfig = SmtpConfig {smtpConfigServer :: Text, smtpConfigUsername :: Text, smtpConfigPassword :: Text}
  deriving stock (Generic, Show)

data SmtpConfigF f = SmtpConfigF
  { smtpConfigFServer :: f Text,
    smtpConfigFUsername :: f Text,
    smtpConfigFPassword :: f Text
  }
  deriving stock (Generic)
  deriving anyclass (FunctorB, ApplicativeB, TraversableB, ConstraintsB)

deriving instance (Show1 f) => Show (SmtpConfigF f)

instance FetchHKD SmtpConfigF where
  type Concrete SmtpConfigF = SmtpConfig

  fromEnv :: SmtpConfigF (Compose IO Maybe)
  fromEnv =
    SmtpConfigF
      { smtpConfigFServer = parseEnvStr "APP_SMTP_SERVER",
        smtpConfigFUsername = parseEnvStr "APP_SMTP_USERNAME",
        smtpConfigFPassword = parseEnvStr "APP_SMTP_PASSWORD"
      }

  toConcrete :: SmtpConfigF (Compose IO Maybe) -> IO (Maybe (Concrete SmtpConfigF))
  toConcrete SmtpConfigF {..} = do
    smtpConfigServer <- getCompose smtpConfigFServer
    smtpConfigUsername <- getCompose smtpConfigFUsername
    smtpConfigPassword <- getCompose smtpConfigFPassword
    pure $ SmtpConfig <$> smtpConfigServer <*> smtpConfigUsername <*> smtpConfigPassword

--------------------------------------------------------------------------------
-- Extended App Config with SMTP

data ExampleAppConfig = ExampleAppConfig
  { exampleCoreConfig :: Core.AppConfig,
    exampleSmtpConfig :: Maybe SmtpConfig
  }
  deriving stock (Generic, Show)

getExampleConfig :: IO (Maybe ExampleAppConfig)
getExampleConfig = do
  coreConfig <- Core.getConfig
  smtpConfig <- toConcrete (fromEnv @SmtpConfigF)
  pure $ ExampleAppConfig <$> coreConfig <*> pure smtpConfig
