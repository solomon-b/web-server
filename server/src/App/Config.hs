{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deriving-defaults #-}

module App.Config where

--------------------------------------------------------------------------------

import App.Config.Fetchers (FetchHKD (..), packText, parseEnv, parseEnvDefault, parseEnvDefaultStr, parseEnvOptional, parseEnvStr, readEnvDefault, readText)
import Barbies
import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.Functor.Classes (Show1)
import Data.Functor.Compose (Compose (..))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Display (Display)
import Data.Word (Word16)
import GHC.Generics

--------------------------------------------------------------------------------

data Environment = Development | Production
  deriving (Generic, Show)
  deriving anyclass (ToJSON)

isProduction :: Environment -> Bool
isProduction = \case
  Development -> False
  Production -> True

--------------------------------------------------------------------------------

newtype Hostname = Hostname {getHostName :: Text}
  deriving (Generic)
  deriving newtype (Show, Display, ToJSON, IsString)

--------------------------------------------------------------------------------

data WarpConfig = WarpConfig
  { warpConfigPort :: Int,
    warpConfigTimeout :: Int,
    warpConfigServerName :: ByteString
  }
  deriving stock (Generic, Show)

data WarpConfigF f = WarpConfigF
  { warpConfigFPort :: f Int,
    warpConfigFTimeout :: f Int,
    warpConfigFServerName :: f ByteString
  }
  deriving stock (Generic)
  deriving anyclass (FunctorB, ApplicativeB, TraversableB, ConstraintsB)

instance FetchHKD WarpConfigF where
  type Concrete WarpConfigF = WarpConfig

  fromEnv :: WarpConfigF (Compose IO Maybe)
  fromEnv =
    WarpConfigF
      { warpConfigFPort = readEnvDefault 3000 "APP_WARP_PORT",
        warpConfigFTimeout = readEnvDefault 30 "APP_WARP_TIMEOUT",
        warpConfigFServerName = parseEnvDefaultStr "server" "APP_WARP_SERVERNAME"
      }

  toConcrete :: WarpConfigF (Compose IO Maybe) -> IO (Maybe (Concrete WarpConfigF))
  toConcrete WarpConfigF {..} = do
    warpConfigPort <- getCompose warpConfigFPort
    warpConfigTimeout <- getCompose warpConfigFTimeout
    warpConfigServerName <- getCompose warpConfigFServerName
    pure $ WarpConfig <$> warpConfigPort <*> warpConfigTimeout <*> warpConfigServerName

--------------------------------------------------------------------------------

data PostgresConfig = PostgresConfig
  { postgresConfigHost :: Maybe ByteString,
    postgresConfigPort :: Maybe Word16,
    postgresConfigDB :: Maybe ByteString,
    postgresConfigUser :: Maybe ByteString,
    postgresConfigPassword :: Maybe ByteString
  }
  deriving stock (Generic, Show)

data PostgresConfigF f = PostgresConfigF
  { postgresConfigFHost :: f (Maybe ByteString),
    postgresConfigFPort :: f (Maybe Word16),
    postgresConfigFDB :: f (Maybe ByteString),
    postgresConfigFUser :: f (Maybe ByteString),
    postgresConfigFPassword :: f (Maybe ByteString)
  }
  deriving stock (Generic)
  deriving anyclass (FunctorB, ApplicativeB, TraversableB, ConstraintsB)

instance FetchHKD PostgresConfigF where
  type Concrete PostgresConfigF = PostgresConfig

  fromEnv :: PostgresConfigF (Compose IO Maybe)
  fromEnv =
    PostgresConfigF
      { postgresConfigFHost = parseEnvOptional packText "APP_POSTGRES_HOST",
        postgresConfigFPort = parseEnvOptional readText "APP_POSTGRES_PORT",
        postgresConfigFDB = parseEnvOptional packText "APP_POSTGRES_DB",
        postgresConfigFUser = parseEnvOptional packText "APP_POSTGRES_USER",
        postgresConfigFPassword = parseEnvOptional packText "APP_POSTGRES_PASSWORD"
      }

  toConcrete :: PostgresConfigF (Compose IO Maybe) -> IO (Maybe (Concrete PostgresConfigF))
  toConcrete PostgresConfigF {..} = do
    postgresConfigHost <- getCompose postgresConfigFHost
    postgresConfigPort <- getCompose postgresConfigFPort
    postgresConfigDB <- getCompose postgresConfigFDB
    postgresConfigUser <- getCompose postgresConfigFUser
    postgresConfigPassword <- getCompose postgresConfigFPassword
    pure $ PostgresConfig <$> postgresConfigHost <*> postgresConfigPort <*> postgresConfigDB <*> postgresConfigUser <*> postgresConfigPassword

--------------------------------------------------------------------------------

data ObservabilityConfig = ObservabilityConfig
  { observabilityConfigVerbosity :: Verbosity,
    observabilityConfigExporter :: AppExporter
  }
  deriving stock (Generic, Show)

-- | https://discourse.ubuntu.com/t/cli-verbosity-levels/26973
--
-- These correspond to log-base log levels as follow:
-- - Quiet: Show @LogAttention@.
-- - Brief: Show @LogInfo@ and @LogAttention@.
-- - Verbose: Show @LogInfo@ and @LogAttention@ (with additional details?)
-- - Debug: Show @LogInfo@, @LogAttention@, and @LogTrace@.
data Verbosity = Quiet | Brief | Verbose | Debug
  deriving stock (Generic, Show)

data AppExporter = StdOut | Otel | None
  deriving (Generic, Show)

data ObservabilityConfigF f = ObservabilityConfigF
  { observabilityConfigFVerbosity :: f Verbosity,
    observabilityConfigFExporter :: f AppExporter
  }
  deriving stock (Generic)
  deriving anyclass (FunctorB, ApplicativeB, TraversableB, ConstraintsB)

instance FetchHKD ObservabilityConfigF where
  type Concrete ObservabilityConfigF = ObservabilityConfig

  fromEnv :: ObservabilityConfigF (Compose IO Maybe)
  fromEnv =
    -- TODO: Case insensitive env parsing:
    ObservabilityConfigF
      { observabilityConfigFVerbosity = parseEnvDefault Brief (\case "Quiet" -> Just Quiet; "Brief" -> Just Brief; "Verbose" -> Just Verbose; "Debug" -> Just Debug; _ -> Just Brief) "APP_OBSERVABILITY_VERBOSITY",
        observabilityConfigFExporter = parseEnvDefault None (\case "StdOut" -> Just StdOut; "Otel" -> Just Otel; _ -> Just None) "APP_OBSERVABILITY_EXPORTER"
      }

  toConcrete :: ObservabilityConfigF (Compose IO Maybe) -> IO (Maybe (Concrete ObservabilityConfigF))
  toConcrete ObservabilityConfigF {..} = do
    observabilityConfigVerbosity <- getCompose observabilityConfigFVerbosity
    observabilityConfigExporter <- getCompose observabilityConfigFExporter
    pure $ ObservabilityConfig <$> observabilityConfigVerbosity <*> observabilityConfigExporter

--------------------------------------------------------------------------------

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

data AppConfig = AppConfig
  { appConfigWarpSettings :: WarpConfig,
    appConfigPostgresSettings :: PostgresConfig,
    appConfigEnvironment :: Environment,
    appConfigObservability :: ObservabilityConfig,
    appConfigSmtp :: Maybe SmtpConfig,
    appConfigHostname :: Hostname
  }
  deriving stock (Generic, Show)

data AppConfigF f = AppConfigF
  { appConfigFWarpSettings :: WarpConfigF f,
    appConfigFPostgresSettings :: PostgresConfigF f,
    appConfigFEnvironment :: f Environment,
    appConfigFObservability :: ObservabilityConfigF f,
    appConfigFSmtp :: SmtpConfigF f,
    appConfigFHostname :: f Hostname
  }
  deriving stock (Generic)
  deriving anyclass (FunctorB, ApplicativeB, TraversableB)

instance FetchHKD AppConfigF where
  type Concrete AppConfigF = AppConfig

  fromEnv :: AppConfigF (Compose IO Maybe)
  fromEnv =
    AppConfigF
      { appConfigFWarpSettings = fromEnv,
        appConfigFEnvironment = parseEnvDefault Development (\case "Development" -> Just Development; "Production" -> Just Production; _ -> Nothing) "APP_ENVIRONMENT",
        appConfigFPostgresSettings = fromEnv,
        appConfigFObservability = fromEnv,
        appConfigFSmtp = fromEnv,
        appConfigFHostname = parseEnvStr "APP_HOSTNAME"
      }

  toConcrete :: AppConfigF (Compose IO Maybe) -> IO (Maybe (Concrete AppConfigF))
  toConcrete AppConfigF {..} = do
    appConfigWarpSettings <- toConcrete appConfigFWarpSettings
    appConfigEnvironment <- getCompose appConfigFEnvironment
    appConfigPostgresSettings <- toConcrete appConfigFPostgresSettings
    appConfigObservability <- toConcrete appConfigFObservability
    appConfigSmtp <- toConcrete appConfigFSmtp
    appConfigHostname <- getCompose appConfigFHostname

    pure $ AppConfig <$> appConfigWarpSettings <*> appConfigPostgresSettings <*> appConfigEnvironment <*> appConfigObservability <*> pure appConfigSmtp <*> appConfigHostname

-- TODO: Replace Maybe with Either
getConfig :: IO (Maybe AppConfig)
getConfig = toConcrete (fromEnv @AppConfigF)
