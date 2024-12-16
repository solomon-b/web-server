{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
-- | Configuration fetching/parsing.
module Config.Fetchers where

--------------------------------------------------------------------------------

import Barbies
import Control.Applicative (Const (..))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Either.Validation
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import System.Environment (lookupEnv)
import Text.Read

--------------------------------------------------------------------------------
-- TODO: Case insensitive env lookup

data Error = MissingEnvVar | ParseFailure
  deriving (Show, Eq)

readText :: (Read a) => Text -> Maybe a
readText = readMaybe . Text.unpack

packText :: Text -> Maybe ByteString
packText = Just . pack . Text.unpack

lookupEnv' :: String -> IO (Either Error String)
lookupEnv' key = lookupEnv key <&> maybe (Left MissingEnvVar) Right

-- | Fetch a required Env Var and then parse it with the provided parser.
parseEnv :: (Text -> Either Error a) -> String -> (IO `Compose` Either Error) a
parseEnv parser envKey =
  Compose $
    lookupEnv' envKey >>= \case
      Left err -> pure $ Left err
      Right val -> pure $ parser $ Text.pack val

-- | Fetch a required Env Var and then parse it with an 'IsString' instance.
parseEnvStr :: (IsString a) => String -> (IO `Compose` Either Error) a
parseEnvStr envKey =
  Compose $
    lookupEnv envKey >>= \case
      Nothing -> pure $ Left MissingEnvVar
      Just val -> pure $ Right $ fromString val

-- | Fetch an optional Env Var and then parse it with the provided parser.
parseEnvOptional :: (Text -> Either Error a) -> String -> (IO `Compose` Either Error) (Maybe a)
parseEnvOptional parser envKey =
  Compose $
    lookupEnv envKey >>= \case
      Nothing -> pure $ Right Nothing
      Just val ->
        case parser $ Text.pack val of
          Left err -> pure $ Left err
          Right a -> pure $ Right $ Just a

-- | Fetch an optional Env Var and then parse it with the provided parser.
parseEnvOptionalStr :: (IsString a) => String -> (IO `Compose` Either Error) (Maybe a)
parseEnvOptionalStr envKey =
  Compose $
    lookupEnv envKey >>= \case
      Nothing -> pure $ Right Nothing
      Just val -> pure $ Right $ Just $ fromString val

-- | Fetch an optional Env Var and then parse it with the provided parser,
-- replacing a missing Env Var with a default value.
parseEnvDefault :: a -> (Text -> Either Error a) -> String -> (IO `Compose` Either Error) a
parseEnvDefault def parser envKey =
  Compose $
    lookupEnv envKey >>= \case
      Nothing -> pure $ Right def
      Just val ->
        case parser $ Text.pack val of
          Left err -> pure $ Left err
          Right a -> pure $ Right a

-- | Fetch an optional Env Var and then parse it using an 'IsString' instance,
-- replacing a missing Env Var with a default value.
parseEnvDefaultStr :: (IsString a) => a -> String -> (IO `Compose` Either Error) a
parseEnvDefaultStr def envKey =
  Compose $
    lookupEnv envKey >>= \case
      Nothing -> pure $ Right def
      Just val -> pure $ Right $ fromString val

-- | Parse a value of type @a@ from an env var using a 'Read' instance.
readEnv :: (Read a) => String -> (IO `Compose` Either Error) a
readEnv envKey =
  Compose $
    lookupEnv' envKey >>= \case
      Left err -> pure $ Left err
      Right val -> pure $ maybe (Left ParseFailure) Right $ readMaybe val

-- | Fetch an optional Env Var and then parse it with a 'Read' instance.
readEnvOptional :: (Read a) => String -> (IO `Compose` Either Error) (Maybe a)
readEnvOptional envKey =
  Compose $
    lookupEnv' envKey >>= \case
      Left _err -> pure $ Right Nothing
      Right val -> pure $ Right $ readMaybe val

-- | Parse a value of type @a@ from an env var using a 'Read' instance with a
-- default value.
readEnvDefault :: (Read a) => a -> String -> (IO `Compose` Either Error) a
readEnvDefault def envKey =
  Compose $
    lookupEnv envKey >>= \case
      Nothing -> pure $ Right def
      Just val -> pure $ maybe (Left ParseFailure) Right $ readMaybe val

--------------------------------------------------------------------------------

class FetchHKD hkd where
  type Concrete hkd :: Type

  fromEnv :: hkd (IO `Compose` Either Error)
  docs :: hkd (Const Text)

  -- fromArg :: hkd (IO `Compose` Either Error)
  -- fromFile :: hkd (IO `Compose` Either Error)

  -- toConcrete :: hkd (IO `Compose` Either Error) -> IO (Either Error (Concrete hkd))

  reify :: hkd Identity -> Concrete hkd

fetchFromEnv :: (FetchHKD hkd, TraversableB hkd) => IO (hkd (Either Error))
fetchFromEnv = bsequence fromEnv

-- |
--
-- Thanks Chris: https://chrispenner.ca/posts/hkd-options#better-errors
validateOptions ::
  (TraversableB b, ApplicativeB b) =>
  b (Const Text) ->
  b (Either Error) ->
  Validation [Text] (b Identity)
validateOptions errMsgs mOpts = bsequence' $ bzipWith validate mOpts errMsgs
  where
    validate :: Either Error a -> Const Text a -> Validation [Text] a
    validate (Right x) _ = Success x
    validate (Left err) (Const doc) = Failure [Text.pack (show err) <> "\n" <> doc]
