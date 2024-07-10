{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.ServerSessions where

--------------------------------------------------------------------------------

import Barbies
import Control.Monad.Identity (Identity (..))
-- import Data.Text.Display (Display (..), RecordInstance (..))

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Data.Time (UTCTime)
import Data.UUID (UUID, fromASCIIBytes)
import Effects.Database.Tables.ServerSessions qualified as ServerSessions
import Effects.Database.Tables.User qualified as User
import Effects.Database.Utils
import GHC.Generics
import Network.IP.Addr (IP, NetAddr)
import OrphanInstances ()
import Rel8 qualified

--------------------------------------------------------------------------------
-- Domain

parseSessionId :: ByteString -> Maybe SessionId
parseSessionId = fmap SessionId . fromASCIIBytes

newtype SessionId = SessionId {toUUID :: UUID}
  deriving newtype (Eq, Show, Display)

data ServerSession = ServerSession
  { serverSessionId :: SessionId,
    serverSessionUserId :: User.Id,
    serverSessionIpAddress :: Maybe (NetAddr IP),
    serverSessionUserAgent :: Maybe Text,
    serverSessionExpiresAt :: UTCTime
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance ServerSession)

instance ModelParser ServerSessions.Model ServerSession where
  parseModel :: ServerSessions.Model Rel8.Result -> ServerSession
  parseModel = parser . runIdentity . bsequence'
    where
      parser :: ServerSessions.Model Identity -> ServerSession
      parser ServerSessions.Model {..} =
        ServerSession
          { serverSessionId = coerce ssId,
            serverSessionUserId = coerce ssUserId,
            serverSessionIpAddress = coerce ssIpAddress,
            serverSessionUserAgent = coerce ssUserAgent,
            serverSessionExpiresAt = coerce ssExpiresAt
          }

instance ModelPrinter ServerSessions.Model (User.Id, Maybe (NetAddr IP), Maybe Text, UTCTime) where
  printModel :: (User.Id, Maybe (NetAddr IP), Maybe Text, UTCTime) -> ServerSessions.Model Rel8.Expr
  printModel (uid, ipAddress, userAgent, expiresAt) =
    ServerSessions.Model
      { ssId = Rel8.unsafeDefault,
        ssUserId = Rel8.litExpr uid,
        ssIpAddress = Rel8.litExpr ipAddress,
        ssUserAgent = Rel8.litExpr userAgent,
        ssExpiresAt = Rel8.litExpr expiresAt
      }
