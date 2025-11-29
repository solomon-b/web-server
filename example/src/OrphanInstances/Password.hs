{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OrphanInstances.Password where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.Password.Argon2 (Password, PasswordHash (..), mkPassword)
import Data.Password.Validate (CharacterCategory (..), InvalidReason (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..), display)
import Hasql.Interpolate (DecodeValue, EncodeValue)

--------------------------------------------------------------------------------

instance Aeson.ToJSON Password where
  toJSON pass = Aeson.String (Text.pack $ show pass)

instance Aeson.FromJSON Password where
  parseJSON = Aeson.withText "Password" (pure . mkPassword)

instance Display Password where
  displayBuilder = displayBuilder . show

instance Display (PasswordHash x) where
  displayBuilder _ = displayBuilder @Text "<PASSWORD_HASH>"

deriving newtype instance DecodeValue (PasswordHash x)

deriving newtype instance EncodeValue (PasswordHash x)

instance Display CharacterCategory where
  displayBuilder = \case
    Uppercase -> "Uppercase letters"
    Lowercase -> "Lowercase letters"
    Special -> "Special characters"
    Digit -> "ASCII digits"

instance Display InvalidReason where
  displayBuilder = \case
    PasswordTooShort ml _ -> displayBuilder @Text $ "Length of Password is too short. Minumum length is " <> display ml <> "."
    PasswordTooLong ml _ -> displayBuilder @Text $ "Length of Password is too long. Maximum length is " <> display ml <> "."
    NotEnoughReqChars cc _ _ -> displayBuilder @Text $ "Password does not contain enough " <> display cc <> "."
    InvalidCharacters chars -> displayBuilder @Text $ "Password contains characters that cannot be used: " <> chars <> "."
