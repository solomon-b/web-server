{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.ServerSessions where

--------------------------------------------------------------------------------

import App.Redact (redacted)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.IP (IPRange)
import Data.Password.Argon2 (Argon2, PasswordHash)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display, RecordInstance (..), ShowInstance (..))
import Data.Time (UTCTime)
import Data.UUID (UUID, fromASCIIBytes)
import Data.UUID qualified as UUID
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

-- | A session id. The id is the value of the session cookie, thus it is a
-- secret.
--
-- 'Display' gives the full id, because the @Set-Cookie@ header needs it.
-- Do not use 'Display' in a log. Use 'App.Auth.truncateSessionId'. 'Show'
-- gives only the first 8 characters, followed by 'redacted'.
newtype Id = Id UUID
  deriving stock (Generic)
  deriving newtype
    ( Eq,
      Ord,
      Servant.FromHttpApiData,
      ToJSON,
      FromJSON,
      Display,
      DecodeValue,
      EncodeValue
    )

instance Show Id where
  showsPrec d (Id uuid) =
    showParen (d > 10) $
      showString "Id " . showString (take 8 (UUID.toString uuid)) . showString (Text.unpack redacted)

-- | 'Show' and 'Display' give only the first 8 characters of the session id.
data Model = Model
  { mSessionId :: Id,
    mUserId :: User.Id,
    mIpAddress :: Maybe IPRange,
    mUserAgent :: Maybe Text,
    mExpiresAt :: UTCTime,
    mLastActivityAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving (Display) via (ShowInstance Model)
  deriving anyclass (DecodeRow)

-- | 'Show' and 'Display' give only the first 8 characters of the session id.
data Domain = Domain
  { dSessionId :: Id,
    dUserId :: User.Id,
    dIpAddress :: Maybe IPRange,
    dUserAgent :: Maybe Text,
    dExpiresAt :: UTCTime,
    dLastActivityAt :: UTCTime
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (ShowInstance Domain)

toDomain :: Model -> Domain
toDomain Model {..} =
  Domain
    { dSessionId = mSessionId,
      dUserId = mUserId,
      dIpAddress = mIpAddress,
      dUserAgent = mUserAgent,
      dExpiresAt = mExpiresAt,
      dLastActivityAt = mLastActivityAt
    }

--------------------------------------------------------------------------------

getServerSession :: Id -> Hasql.Statement () (Maybe Model)
getServerSession sid =
  interp
    False
    [sql|
    SELECT id, user_id, ip_address, user_agent, expires_at, last_activity_at
    FROM server_sessions
    WHERE id = #{sid}
      AND NOW() < expires_at
  |]

getSessionUser :: Id -> Hasql.Statement () (Maybe (User.Model, Model))
getSessionUser sId =
  fmap fromRows
    <$> interp
      False
      [sql|
    SELECT
      u.id as user_id, u.email, u.password,
      s.id as server_session_id, s.user_id as session_user_id, s.ip_address, s.user_agent, s.expires_at, s.last_activity_at
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
        Id,
        User.Id,
        Maybe IPRange,
        Maybe Text,
        UTCTime,
        UTCTime
      ) ->
      (User.Model, Model)
    fromRows
      ( mId,
        mEmail,
        mPassword,
        mSessionId,
        sessionUserId,
        mIpAddress,
        mUserAgent,
        mExpiresAt,
        mLastActivityAt
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
    WITH expired AS (
      UPDATE server_sessions
      SET expires_at = NOW()
      WHERE user_id = #{ssiUserId}
        AND expires_at > NOW()
        AND id NOT IN (
          SELECT id FROM server_sessions
          WHERE user_id = #{ssiUserId}
            AND expires_at > NOW()
          ORDER BY expires_at DESC
          LIMIT 4
          FOR UPDATE
        )
    )
    INSERT INTO server_sessions(user_id, ip_address, user_agent, expires_at)
    VALUES (#{ssiUserId}, #{ssiIpAddress}, #{ssiUserAgent}, #{ssiExpiresAt})
    RETURNING id, user_id, ip_address, user_agent, expires_at, last_activity_at
  |]

touchSession :: Id -> Hasql.Statement () ()
touchSession sessionId =
  interp
    False
    [sql|
    UPDATE server_sessions
    SET last_activity_at = NOW()
    WHERE id = #{sessionId}
      AND NOW() < expires_at
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
