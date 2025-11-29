{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.UserMetadata where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.FullName (FullName)
import Effects.Database.Tables.User qualified as User
import GHC.Generics
import Hasql.Interpolate (DecodeRow, DecodeValue, EncodeRow, EncodeValue, OneRow, interp, sql)
import Hasql.Statement qualified as Hasql
import Servant qualified

--------------------------------------------------------------------------------
-- Model

newtype Id = Id Int64
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype
    ( Show,
      Eq,
      Ord,
      Num,
      Servant.FromHttpApiData,
      Servant.ToHttpApiData,
      ToJSON,
      FromJSON,
      Display,
      DecodeValue,
      EncodeValue
    )

-- | Database Model for the @user_metadata@ table.
data Model = Model
  { mId :: Id,
    mUserId :: User.Id,
    mDisplayName :: DisplayName,
    mFullName :: FullName,
    mAvatarUrl :: Maybe Text,
    mIsAdmin :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

-- | API Domain Type for @UserMetadata@.
data Domain = Domain
  { dId :: Id,
    dUserId :: User.Id,
    dDisplayName :: DisplayName,
    dFullName :: FullName,
    dAvatarUrl :: Maybe Text,
    dIsAdmin :: Bool
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance Domain)
  deriving anyclass (FromJSON, ToJSON)

toDomain :: Model -> Domain
toDomain Model {..} =
  Domain
    { dId = mId,
      dUserId = mUserId,
      dDisplayName = mDisplayName,
      dFullName = mFullName,
      dAvatarUrl = mAvatarUrl,
      dIsAdmin = mIsAdmin
    }

--------------------------------------------------------------------------------

getByUserId :: User.Id -> Hasql.Statement () (Maybe Model)
getByUserId userId =
  interp
    False
    [sql|
    SELECT id, user_id, display_name, full_name, avatar_url, is_admin
    FROM user_metadata
    WHERE user_id = #{userId}
  |]

getById :: Id -> Hasql.Statement () (Maybe Model)
getById metadataId =
  interp
    False
    [sql|
    SELECT id, user_id, display_name, full_name, avatar_url, is_admin
    FROM user_metadata
    WHERE id = #{metadataId}
  |]

data ModelInsert = ModelInsert
  { miUserId :: User.Id,
    miDisplayName :: DisplayName,
    miFullName :: FullName,
    miAvatarUrl :: Maybe Text,
    miIsAdmin :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via ModelInsert
  deriving (Display) via (RecordInstance ModelInsert)

insert :: ModelInsert -> Hasql.Statement () (OneRow Id)
insert ModelInsert {..} =
  interp
    False
    [sql|
    INSERT INTO user_metadata(user_id, display_name, full_name, avatar_url, is_admin)
    VALUES (#{miUserId}, #{miDisplayName}, #{miFullName}, #{miAvatarUrl}, #{miIsAdmin})
    RETURNING id
  |]

data ModelUpdate = ModelUpdate
  { muDisplayName :: DisplayName,
    muFullName :: FullName,
    muAvatarUrl :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via ModelUpdate
  deriving (Display) via (RecordInstance ModelUpdate)

updateByUserId :: User.Id -> ModelUpdate -> Hasql.Statement () ()
updateByUserId userId ModelUpdate {..} =
  interp
    False
    [sql|
    UPDATE user_metadata
    SET display_name = #{muDisplayName},
        full_name = #{muFullName},
        avatar_url = #{muAvatarUrl},
        updated_at = NOW()
    WHERE user_id = #{userId}
  |]

deleteByUserId :: User.Id -> Hasql.Statement () ()
deleteByUserId userId =
  interp
    False
    [sql|
    DELETE FROM user_metadata
    WHERE user_id = #{userId}
  |]

setAdminStatus :: User.Id -> Bool -> Hasql.Statement () ()
setAdminStatus userId isAdmin =
  interp
    False
    [sql|
    UPDATE user_metadata
    SET is_admin = #{isAdmin},
        updated_at = NOW()
    WHERE user_id = #{userId}
  |]
