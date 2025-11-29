{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Queries.UserWithMetadata where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.IP (IPRange)
import Data.Password.Argon2 (Argon2, PasswordHash)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Data.Time (UTCTime)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import Effects.Database.Tables.ServerSessions qualified as Session
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import GHC.Generics
import Hasql.Interpolate (DecodeRow, interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.IPRange ()
import OrphanInstances.Password ()
import OrphanInstances.UTCTime ()
import OrphanInstances.UUID ()
import Servant qualified

--------------------------------------------------------------------------------
-- FullUser - Combined User + UserMetadata

-- | Full user data combining the core user table with user_metadata.
-- This is the type used throughout the application for displaying user info.
data FullUser = FullUser
  { fuId :: User.Id,
    fuEmail :: EmailAddress,
    fuDisplayName :: DisplayName,
    fuFullName :: FullName,
    fuAvatarUrl :: Maybe Text,
    fuIsAdmin :: Bool
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance FullUser)
  deriving anyclass (FromJSON, ToJSON)

-- | Construct a FullUser from User.Model and UserMetadata.Model
fromModels :: User.Model -> UserMetadata.Model -> FullUser
fromModels User.Model {..} UserMetadata.Model {mDisplayName, mFullName, mAvatarUrl, mIsAdmin} =
  FullUser
    { fuId = mId,
      fuEmail = mEmail,
      fuDisplayName = mDisplayName,
      fuFullName = mFullName,
      fuAvatarUrl = mAvatarUrl,
      fuIsAdmin = mIsAdmin
    }

isAdmin :: FullUser -> Bool
isAdmin = fuIsAdmin

--------------------------------------------------------------------------------
-- Queries

getFullUsers :: Hasql.Statement () [FullUser]
getFullUsers =
  fmap fromRow
    <$> interp
      False
      [sql|
    SELECT u.id, u.email, m.display_name, m.full_name, m.avatar_url, m.is_admin
    FROM users u
    JOIN user_metadata m ON m.user_id = u.id
  |]
  where
    fromRow :: (User.Id, EmailAddress, DisplayName, FullName, Maybe Text, Bool) -> FullUser
    fromRow (fuId, fuEmail, fuDisplayName, fuFullName, fuAvatarUrl, fuIsAdmin) = FullUser {..}

getFullUser :: User.Id -> Hasql.Statement () (Maybe FullUser)
getFullUser userId =
  fmap fromRow
    <$> interp
      False
      [sql|
    SELECT u.id, u.email, m.display_name, m.full_name, m.avatar_url, m.is_admin
    FROM users u
    JOIN user_metadata m ON m.user_id = u.id
    WHERE u.id = #{userId}
  |]
  where
    fromRow :: (User.Id, EmailAddress, DisplayName, FullName, Maybe Text, Bool) -> FullUser
    fromRow (fuId, fuEmail, fuDisplayName, fuFullName, fuAvatarUrl, fuIsAdmin) = FullUser {..}

-- | Get user by email, returning both User.Model (for password checking) and FullUser (for display)
getFullUserByEmail :: EmailAddress -> Hasql.Statement () (Maybe (User.Model, FullUser))
getFullUserByEmail email =
  fmap fromRow
    <$> interp
      False
      [sql|
    SELECT u.id, u.email, u.password, m.display_name, m.full_name, m.avatar_url, m.is_admin
    FROM users u
    JOIN user_metadata m ON m.user_id = u.id
    WHERE u.email = #{email}
  |]
  where
    fromRow :: (User.Id, EmailAddress, PasswordHash Argon2, DisplayName, FullName, Maybe Text, Bool) -> (User.Model, FullUser)
    fromRow (userId, userEmail, password, displayName, fullName, avatarUrl, isAdminVal) =
      ( User.Model {mId = userId, mEmail = userEmail, mPassword = password},
        FullUser
          { fuId = userId,
            fuEmail = userEmail,
            fuDisplayName = displayName,
            fuFullName = fullName,
            fuAvatarUrl = avatarUrl,
            fuIsAdmin = isAdminVal
          }
      )

--------------------------------------------------------------------------------
-- Session queries with FullUser

-- | Get session with full user data (used for authentication)
getSessionFullUser :: Session.Id -> Hasql.Statement () (Maybe (FullUser, Session.Model))
getSessionFullUser sId =
  fmap fromRow
    <$> interp
      False
      [sql|
    SELECT
      u.id, u.email, m.display_name, m.full_name, m.avatar_url, m.is_admin,
      s.id, s.user_id, s.ip_address, s.user_agent, s.expires_at
    FROM server_sessions s
    JOIN users u ON u.id = s.user_id
    JOIN user_metadata m ON m.user_id = u.id
    WHERE s.id = #{sId}
      AND NOW() < s.expires_at
  |]
  where
    fromRow ::
      ( User.Id,
        EmailAddress,
        DisplayName,
        FullName,
        Maybe Text,
        Bool,
        Session.Id,
        User.Id,
        Maybe IPRange,
        Maybe Text,
        UTCTime
      ) ->
      (FullUser, Session.Model)
    fromRow
      ( userId,
        userEmail,
        displayName,
        fullName,
        avatarUrl,
        isAdminVal,
        sessionId,
        sessionUserId,
        ipAddress,
        userAgent,
        expiresAt
        ) =
        ( FullUser
            { fuId = userId,
              fuEmail = userEmail,
              fuDisplayName = displayName,
              fuFullName = fullName,
              fuAvatarUrl = avatarUrl,
              fuIsAdmin = isAdminVal
            },
          Session.Model
            { Session.mSessionId = sessionId,
              Session.mUserId = sessionUserId,
              Session.mIpAddress = ipAddress,
              Session.mUserAgent = userAgent,
              Session.mExpiresAt = expiresAt
            }
        )
