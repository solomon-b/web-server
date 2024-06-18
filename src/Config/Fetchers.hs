{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deriving-defaults #-}

-- | Configuration fetching/parsing.
module Config.Fetchers where

--------------------------------------------------------------------------------

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Functor.Barbie (TraversableB, bsequence)
import Data.Functor.Compose (Compose (..))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Environment (lookupEnv)
import Text.Read

--------------------------------------------------------------------------------

readText :: (Read a) => Text -> Maybe a
readText = readMaybe . Text.unpack

packText :: Text -> Maybe ByteString
packText = Just . pack . Text.unpack

readEnv :: (Text -> a) -> String -> (IO `Compose` Maybe) a
readEnv cstr envKey = Compose $ fmap (cstr . Text.pack) <$> lookupEnv envKey

injectFunctor :: (Functor f, Applicative h) => Compose f g a -> Compose f h (g a)
injectFunctor = Compose . fmap pure . getCompose

readEnvOptional :: (Text -> Maybe a) -> String -> Compose IO Maybe (Maybe a)
readEnvOptional cstr envKey = Compose $ do
  lookupEnv envKey >>= \case
    Nothing -> pure $ Just Nothing
    Just val -> pure $ Just $ cstr $ Text.pack val

readEnvDefault :: a -> (Text -> Maybe a) -> String -> (IO `Compose` Maybe) a
readEnvDefault def cstr envKey = Compose $ fmap (fromMaybe def . cstr . Text.pack) <$> lookupEnv envKey

--------------------------------------------------------------------------------

class FetchHKD hkd where
  type Concrete hkd :: Type

  fromEnv :: hkd (Compose IO Maybe)
  -- fromArg :: hkd (Compose IO Maybe)
  -- fromFile :: hkd (Compose IO Maybe)
  toConcrete :: (Applicative f) => hkd f -> f (Concrete hkd)

fetchFromEnv :: (FetchHKD hkd, TraversableB hkd) => IO (hkd Maybe)
fetchFromEnv = bsequence fromEnv
