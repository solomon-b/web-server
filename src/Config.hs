{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deriving-defaults #-}

module Config where

--------------------------------------------------------------------------------

import Barbies
import Config.Fetchers (FetchHKD (..), packText, readEnv, readEnvDefault, readEnvOptional, readText)
import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Functor.Compose (Compose)
import Data.Text (Text)
import Data.Text qualified as Text
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

newtype Hostname = Hostname Text
  deriving (Generic)
  deriving newtype (Show, Display, ToJSON)

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
      { warpConfigFPort = readEnv (read . Text.unpack) "APP_WARP_PORT",
        warpConfigFTimeout = readEnv (read . Text.unpack) "APP_WARP_TIMEOUT",
        warpConfigFServerName = readEnv (pack . Text.unpack) "APP_WARP_SERVERNAME"
      }

  toConcrete :: (Applicative f) => WarpConfigF f -> f WarpConfig
  toConcrete WarpConfigF {..} = WarpConfig <$> warpConfigFPort <*> warpConfigFTimeout <*> warpConfigFServerName

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
      { postgresConfigFHost = readEnvOptional packText "APP_POSTGRES_HOST",
        postgresConfigFPort = readEnvOptional readText "APP_POSTGRES_PORT",
        postgresConfigFDB = readEnvOptional packText "APP_POSTGRES_DB",
        postgresConfigFUser = readEnvOptional packText "APP_POSTGRES_USER",
        postgresConfigFPassword = readEnvOptional packText "APP_POSTGRES_PASSWORD"
      }

  toConcrete :: (Applicative f) => PostgresConfigF f -> f PostgresConfig
  toConcrete PostgresConfigF {..} = PostgresConfig <$> postgresConfigFHost <*> postgresConfigFPort <*> postgresConfigFDB <*> postgresConfigFUser <*> postgresConfigFPassword

--------------------------------------------------------------------------------

data ObservabilityConfig = ObservabilityConfig
  { observabilityConfigVerbosity :: Verbosity,
    observabilityConfigExporter :: AppExporter
  }
  deriving stock (Generic, Show)

data Verbosity = Quiet | Loud
  deriving stock (Generic, Show)

data AppExporter = StdOut | Otel
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
    ObservabilityConfigF
      { observabilityConfigFVerbosity = readEnvDefault Quiet (\case "Quiet" -> Just Quiet; "Loud" -> Just Loud; _ -> Nothing) "APP_OBSERVABILITY_VERBOSITY",
        observabilityConfigFExporter = readEnvDefault StdOut (\case "StdOut" -> Just StdOut; _ -> Nothing) "APP_OBSERVABILITY_EXPORTER"
      }

  toConcrete :: (Applicative f) => ObservabilityConfigF f -> f ObservabilityConfig
  toConcrete ObservabilityConfigF {..} = ObservabilityConfig <$> observabilityConfigFVerbosity <*> observabilityConfigFExporter

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

instance FetchHKD SmtpConfigF where
  type Concrete SmtpConfigF = SmtpConfig

  fromEnv :: SmtpConfigF (Compose IO Maybe)
  fromEnv =
    SmtpConfigF
      { smtpConfigFServer = readEnv id "APP_SMTP_SERVER",
        smtpConfigFUsername = readEnv id "APP_SMTP_USERNAME",
        smtpConfigFPassword = readEnv id "APP_SMTP_PASSWORD"
      }

  toConcrete :: (Applicative f) => SmtpConfigF f -> f SmtpConfig
  toConcrete SmtpConfigF {..} = SmtpConfig <$> smtpConfigFServer <*> smtpConfigFUsername <*> smtpConfigFPassword

--------------------------------------------------------------------------------

data AppConfig = AppConfig
  { appConfigWarpSettings :: WarpConfig,
    appConfigPostgresSettings :: PostgresConfig,
    appConfigEnvironment :: Environment,
    appConfigObservability :: ObservabilityConfig,
    appConfigSmtp :: SmtpConfig,
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
  deriving anyclass (FunctorB, ApplicativeB, TraversableB, ConstraintsB)

instance FetchHKD AppConfigF where
  type Concrete AppConfigF = AppConfig

  fromEnv :: AppConfigF (Compose IO Maybe)
  fromEnv =
    AppConfigF
      { appConfigFWarpSettings = fromEnv,
        appConfigFEnvironment = readEnvDefault Development (\case "Development" -> Just Development; "Production" -> Just Production; _ -> Nothing) "APP_ENVIRONMENT",
        appConfigFPostgresSettings = fromEnv,
        appConfigFObservability = fromEnv,
        appConfigFSmtp = fromEnv,
        appConfigFHostname = readEnv Hostname "APP_HOSTNAME"
      }

  toConcrete :: (Applicative f) => AppConfigF f -> f AppConfig
  toConcrete AppConfigF {..} =
    AppConfig <$> toConcrete appConfigFWarpSettings <*> toConcrete appConfigFPostgresSettings <*> appConfigFEnvironment <*> toConcrete appConfigFObservability <*> toConcrete appConfigFSmtp <*> appConfigFHostname

getConfig :: IO (Maybe AppConfig)
getConfig = toConcrete @AppConfigF <$> bsequence fromEnv
