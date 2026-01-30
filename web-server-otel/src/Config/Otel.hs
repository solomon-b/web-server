{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deriving-defaults #-}

module Config.Otel
  ( -- * Types
    ObservabilityConfig (..),
    AppExporter (..),

    -- * Config fetching
    ObservabilityConfigF (..),
  )
where

--------------------------------------------------------------------------------

import App.Config.Fetchers (FetchHKD (..), parseEnvDefault)
import Barbies
import Data.Functor.Compose (Compose (..))
import GHC.Generics

--------------------------------------------------------------------------------

data AppExporter = StdOut | Otel | None
  deriving (Generic, Show)

newtype ObservabilityConfig = ObservabilityConfig
  { observabilityConfigExporter :: AppExporter
  }
  deriving stock (Generic, Show)

newtype ObservabilityConfigF f = ObservabilityConfigF
  { observabilityConfigFExporter :: f AppExporter
  }
  deriving stock (Generic)
  deriving anyclass (FunctorB, ApplicativeB, TraversableB, ConstraintsB)

instance FetchHKD ObservabilityConfigF where
  type Concrete ObservabilityConfigF = ObservabilityConfig

  fromEnv :: ObservabilityConfigF (Compose IO Maybe)
  fromEnv =
    -- TODO: Case insensitive env parsing:
    ObservabilityConfigF
      { observabilityConfigFExporter = parseEnvDefault None (\case "StdOut" -> Just StdOut; "Otel" -> Just Otel; _ -> Nothing) "APP_OBSERVABILITY_EXPORTER"
      }

  toConcrete :: ObservabilityConfigF (Compose IO Maybe) -> IO (Maybe (Concrete ObservabilityConfigF))
  toConcrete ObservabilityConfigF {..} = do
    observabilityConfigExporter <- getCompose observabilityConfigFExporter
    pure $ ObservabilityConfig <$> observabilityConfigExporter
