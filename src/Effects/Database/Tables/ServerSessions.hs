{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.ServerSessions where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.IP (IPRange)
import Data.Password.Argon2 (Argon2, PasswordHash)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Data.Time (UTCTime)
import Data.UUID (UUID, fromASCIIBytes)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Effects.Database.Tables.User qualified as User
import GHC.Generics
import Hasql.Interpolate (DecodeRow, DecodeValue, EncodeRow, EncodeValue, OneRow, interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.IPRange ()
import OrphanInstances.UTCTime ()
import OrphanInstances.UUID ()
import Servant qualified

--------------------------------------------------------------------------------
-- Model

parseSessionId :: ByteString -> Maybe Id
parseSessionId = fmap Id . fromASCIIBytes

newtype Id = Id UUID
  deriving stock (Generic)
  deriving newtype
    ( Show,
      Eq,
      Ord,
      Servant.FromHttpApiData,
      ToJSON,
      FromJSON,
      Display,
      DecodeValue,
      EncodeValue
    )

data Model = Model
  { mSessionId :: Id,
    mUserId :: User.Id,
    mIpAddress :: Maybe IPRange,
    mUserAgent :: Maybe Text,
    mExpiresAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving (Display) via (RecordInstance Model)
  deriving anyclass (DecodeRow)

data Domain = Domain
  { dSessionId :: Id,
    dUserId :: User.Id,
    dIpAddress :: Maybe IPRange,
    dUserAgent :: Maybe Text,
    dExpiresAt :: UTCTime
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance Domain)

toDomain :: Model -> Domain
toDomain Model {..} =
  Domain
    { dSessionId = mSessionId,
      dUserId = mUserId,
      dIpAddress = mIpAddress,
      dUserAgent = mUserAgent,
      dExpiresAt = mExpiresAt
    }

--------------------------------------------------------------------------------

getServerSession :: Id -> Hasql.Statement () (Maybe Model)
getServerSession sid =
  interp
    False
    [sql|
    SELECT id, user_id, ip_address, user_agent, expires_at
    FROM server_sessions
    WHERE id = #{sid} 
      AND NOW() < expires_at
  |]

getServerSessionByUser :: User.Id -> Hasql.Statement () (Maybe Model)
getServerSessionByUser userId =
  interp
    False
    [sql|
    SELECT id, user_id, ip_address, user_agent, expires_at
    FROM server_sessions
    WHERE user_id = #{userId}
      AND NOW() < expires_at
  |]

getSessionUser :: Id -> Hasql.Statement () (Maybe (User.Model, Model))
getSessionUser sId =
  fmap fromRows
    <$> interp
      False
      [sql|
    SELECT
      u.id as user_id, u.email, u.password, u.display_name, u.avatar_url, u.is_admin,
      s.id as server_session_id, s.user_id as session_user_id, s.ip_address, s.user_agent, s.expires_at
    FROM server_sessions s
    JOIN users u ON u.id = s.user_id
    WHERE s.id = #{sId}
      AND NOW() < expires_at
  |]
  where
    fromRows ::
      ( User.Id,
        EmailAddress,
        PasswordHash Argon2,
        DisplayName,
        Maybe Text,
        Bool,
        Id,
        User.Id,
        Maybe IPRange,
        Maybe Text,
        UTCTime
      ) ->
      (User.Model, Model)
    fromRows
      ( mId,
        mEmail,
        mPassword,
        mDisplayName,
        mAvatarUrl,
        mIsAdmin,
        mSessionId,
        sessionUserId,
        mIpAddress,
        mUserAgent,
        mExpiresAt
        ) = (User.Model {..}, Model {mUserId = sessionUserId, ..})

data ServerSessionInsert = ServerSessionInsert
  { ssiUserId :: User.Id,
    ssiIpAddress :: Maybe IPRange,
    ssiUserAgent :: Maybe Text,
    ssiExpiresAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving (Display) via (RecordInstance ServerSessionInsert)
  deriving anyclass (EncodeRow)

insertServerSession :: ServerSessionInsert -> Hasql.Statement () (OneRow Model)
insertServerSession ServerSessionInsert {..} =
  interp
    False
    [sql|
    INSERT INTO server_sessions(user_id, ip_address, user_agent, expires_at)
    VALUES (#{ssiUserId}, #{ssiIpAddress}, #{ssiUserAgent}, #{ssiExpiresAt})
    RETURNING id, user_id, ip_address, user_agent, expires_at
  |]

expireSession :: Id -> Hasql.Statement () ()
expireSession sessionId =
  interp
    False
    [sql|
    UPDATE server_sessions
    SET expires_at = NOW()
    WHERE id = #{sessionId}
  |]
