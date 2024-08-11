{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OrphanInstances where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Functor.Contravariant (contramap)
import Data.IP (IPRange)
import Data.Password.Argon2 (Password, PasswordHash (..), mkPassword)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..), ShowInstance (..))
import Data.Text.Internal.Builder qualified as Text
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Hasql.Interpolate (DecodeValue (..), EncodeValue (..), OneRow (..))
import Lucid.Base qualified
import Network.IP.Addr (IP, NetAddr)
import Servant qualified

--------------------------------------------------------------------------------

instance (Display a) => Display (Servant.Headers x a) where
  displayBuilder :: (Display a) => Servant.Headers x a -> Text.Builder
  displayBuilder Servant.Headers {..} = displayBuilder getResponse

instance Display Servant.NoContent where
  displayBuilder :: Servant.NoContent -> Text.Builder
  displayBuilder Servant.NoContent = displayBuilder ()

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

deriving newtype instance DecodeValue (PasswordHash x)

deriving newtype instance EncodeValue (PasswordHash x)

deriving via (ShowInstance UUID) instance (Display UUID)

deriving via (ShowInstance (NetAddr IP)) instance (Display (NetAddr IP))

deriving via (ShowInstance IPRange) instance (Display IPRange)

deriving via (ShowInstance UTCTime) instance (Display UTCTime)

deriving via (ShowInstance (Lucid.Base.Html ())) instance (Display (Lucid.Base.Html ()))

instance EncodeValue (CI Text) where
  encodeValue = contramap CI.original (encodeValue @Text)

instance DecodeValue (CI Text) where
  decodeValue = fmap CI.mk (decodeValue @Text)

instance (Display a) => Display (OneRow a) where
  displayBuilder = displayBuilder . getOneRow
