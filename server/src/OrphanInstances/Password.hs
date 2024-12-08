{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OrphanInstances.Password where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.Password.Argon2 (Password, PasswordHash (..), mkPassword)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..))
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
