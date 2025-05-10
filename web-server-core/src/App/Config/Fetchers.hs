{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deriving-defaults #-}

-- | Configuration fetching/parsing.
module App.Config.Fetchers where

--------------------------------------------------------------------------------

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Functor.Barbie (TraversableB, bsequence)
import Data.Functor.Compose (Compose (..))
import Data.Kind (Type)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import System.Environment (lookupEnv)
import Text.Read

--------------------------------------------------------------------------------

{-

parseEnv = fetch a required env var and then parse it. Either failing is an error
parseEnvOptional = fetch an optional env var and then parse it. Env Fetch failure is okay.
parseEnvDefault = fetch a required env var and then parse it. Return default if env fetch fails.

-}
readText :: (Read a) => Text -> Maybe a
readText = readMaybe . Text.unpack

packText :: Text -> Maybe ByteString
packText = Just . pack . Text.unpack

-- | Parse a value of type @a@ from an env var.
parseEnv :: (Text -> a) -> String -> (IO `Compose` Maybe) a
parseEnv parser envKey = Compose $ fmap (parser . Text.pack) <$> lookupEnv envKey

-- | Fetch an optional Env Var and then parse it with the provided parser.
parseEnvOptional :: (Text -> Maybe a) -> String -> Compose IO Maybe (Maybe a)
parseEnvOptional parser envKey = Compose $ do
  lookupEnv envKey >>= \case
    Nothing -> pure $ Just Nothing
    Just val -> pure $ Just $ parser $ Text.pack val

-- | Fetch a required Env Var and then parse it with an 'IsString' instance.
parseEnvStr :: (IsString a) => String -> (IO `Compose` Maybe) a
parseEnvStr envKey = Compose $ do
  lookupEnv envKey >>= \case
    Nothing -> pure Nothing
    Just val -> pure $ Just $ fromString val

-- | Fetch an optional Env Var and then parse it with the provided parser,
-- replacing a missing Env Var with a default value.
parseEnvDefault :: a -> (Text -> Maybe a) -> String -> (IO `Compose` Maybe) a
parseEnvDefault def parser envKey = Compose $ do
  lookupEnv envKey >>= \case
    Nothing -> pure $ Just def
    Just val -> pure $ parser $ Text.pack val

-- | Fetch an optional Env Var and then parse it using an 'IsString' instance,
-- replacing a missing Env Var with a default value.
parseEnvDefaultStr :: (IsString a) => a -> String -> (IO `Compose` Maybe) a
parseEnvDefaultStr def envKey = Compose $ do
  lookupEnv envKey >>= \case
    Nothing -> pure $ Just def
    Just val -> pure $ Just $ fromString val

-- | Parse a value of type @a@ from an env var using a 'Read' instance.
readEnv :: (Read a) => String -> (IO `Compose` Maybe) a
readEnv envKey = Compose $ fmap read <$> lookupEnv envKey

-- | Parse a value of type @a@ from an env var using a 'Read' instance with a
-- default value.
readEnvDefault :: (Read a) => a -> String -> (IO `Compose` Maybe) a
readEnvDefault def envKey = Compose $ do
  lookupEnv envKey >>= \case
    Nothing -> pure $ Just def
    Just val -> pure $ readMaybe val

--------------------------------------------------------------------------------

class FetchHKD hkd where
  type Concrete hkd :: Type

  fromEnv :: hkd (Compose IO Maybe)

  -- fromArg :: hkd (Compose IO Maybe)
  -- fromFile :: hkd (Compose IO Maybe)

  toConcrete :: hkd (Compose IO Maybe) -> IO (Maybe (Concrete hkd))

fetchFromEnv :: (FetchHKD hkd, TraversableB hkd) => IO (hkd Maybe)
fetchFromEnv = bsequence fromEnv
