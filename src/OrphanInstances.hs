{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OrphanInstances where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.Password.Argon2 (Password, PasswordHash (..), mkPassword)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..), ShowInstance (..))
import Data.Text.Internal.Builder qualified as Text
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Network.IP.Addr (IP, NetAddr)
import Rel8 qualified
import Servant qualified

--------------------------------------------------------------------------------

instance (Display a) => Display (Servant.Headers x a) where
  displayBuilder :: (Display a) => Servant.Headers x a -> Text.Builder
  displayBuilder Servant.Headers {..} = displayBuilder getResponse

instance Display Servant.NoContent where
  displayBuilder :: Servant.NoContent -> Text.Builder
  displayBuilder Servant.NoContent = displayBuilder ()

instance Rel8.DBType (PasswordHash x) where
  typeInformation =
    Rel8.TypeInformation
      { encode = Rel8.encode (Rel8.typeInformation @Text) . unPasswordHash,
        decode = PasswordHash <$> Rel8.decode Rel8.typeInformation,
        typeName = Rel8.typeName (Rel8.typeInformation @Text)
      }

instance Rel8.DBEq (PasswordHash x)

instance Aeson.ToJSON Password where
  toJSON pass = Aeson.String (Text.pack $ show pass)

instance Aeson.FromJSON Password where
  parseJSON = Aeson.withText "Password" (pure . mkPassword)

instance Display Password where
  displayBuilder = displayBuilder . show

instance Display (PasswordHash x) where
  displayBuilder _ = displayBuilder @Text "<PASSWORD_HASH>"

instance Aeson.ToJSON (PasswordHash x) where
  toJSON pass = Aeson.String (unPasswordHash pass)

deriving via (ShowInstance UUID) instance (Display UUID)

deriving via (ShowInstance (NetAddr IP)) instance (Display (NetAddr IP))

deriving via (ShowInstance UTCTime) instance (Display UTCTime)
