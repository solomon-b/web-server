module Effects.Database.Tables.ServerSessions where

--------------------------------------------------------------------------------

import Barbies
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Effects.Database.Tables.User qualified as User
import GHC.Generics
import Network.IP.Addr (IP, NetAddr)
import OrphanInstances ()
import Rel8 qualified
import Servant qualified
import Servant.Auth.JWT (FromJWT, ToJWT)

--------------------------------------------------------------------------------
-- Model

newtype Id = Id UUID
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Servant.FromHttpApiData, Rel8.DBEq, Rel8.DBType, ToJSON, FromJSON)
  deriving anyclass (ToJWT, FromJWT)

-- | Database Model for the `user` table.
data Model f = Model
  { ssId :: f Id,
    ssUserId :: f User.Id,
    ssIpAddress :: f (Maybe (NetAddr IP)),
    ssUserAgent :: f (Maybe Text),
    ssExpiresAt :: f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8.Rel8able, FunctorB, TraversableB, ApplicativeB, ConstraintsB)

schema :: Rel8.TableSchema (Model Rel8.Name)
schema =
  Rel8.TableSchema
    { Rel8.name = "server_sessions",
      Rel8.columns =
        Model
          { ssId = "id",
            ssUserId = "user_id",
            ssIpAddress = "ip_address",
            ssUserAgent = "user_agent",
            ssExpiresAt = "expires_at"
          }
    }
