{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.Database.Tables.User where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Password.Argon2 (Argon2, PasswordHash)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import GHC.Generics
import Hasql.Interpolate (DecodeRow, DecodeValue, EncodeRow, EncodeValue, OneRow, interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Password ()
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
      ToJSON,
      FromJSON,
      Display,
      DecodeValue,
      EncodeValue
    )

-- | Database Model for the @user@ table.
data Model = Model
  { mId :: Id,
    mEmail :: EmailAddress,
    mPassword :: PasswordHash Argon2,
    mDisplayName :: DisplayName,
    mFullName :: FullName,
    mAvatarUrl :: Maybe Text,
    mIsAdmin :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

-- | API Domain Type for @Users@.
data Domain = Domain
  { dId :: Id,
    dEmail :: EmailAddress,
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
      dEmail = mEmail,
      dDisplayName = mDisplayName,
      dFullName = mFullName,
      dAvatarUrl = mAvatarUrl,
      dIsAdmin = mIsAdmin
    }

--------------------------------------------------------------------------------

getUsers :: Hasql.Statement () [Model]
getUsers =
  interp
    False
    [sql|
    SELECT id, email, password, display_name, full_name, avatar_url, is_admin
    FROM users
  |]

getUser :: Id -> Hasql.Statement () (Maybe Model)
getUser userId =
  interp
    False
    [sql|
    SELECT id, email, password, display_name, full_name, avatar_url, is_admin
    FROM users
    WHERE id = #{userId} 
  |]

getUserByEmail :: EmailAddress -> Hasql.Statement () (Maybe Model)
getUserByEmail email =
  interp
    False
    [sql|
    SELECT id, email, password, display_name, full_name, avatar_url, is_admin
    FROM users
    WHERE email = #{email} 
  |]

getUserByCredential :: EmailAddress -> PasswordHash Argon2 -> Hasql.Statement () (Maybe Model)
getUserByCredential email password =
  interp
    False
    [sql|
      SELECT id, email, password, display_name, full_name, avatar_url, is_admin
      FROM users
      WHERE email = #{email} && password = #{password}
    |]

data ModelInsert = ModelInsert
  { miEmail :: EmailAddress,
    miPassword :: PasswordHash Argon2,
    miDisplayName :: DisplayName,
    miFullName :: FullName,
    miAvatarUrl :: Maybe Text,
    miIsAdmin :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via ModelInsert
  deriving (Display) via (RecordInstance ModelInsert)

insertUser :: ModelInsert -> Hasql.Statement () (OneRow Id)
insertUser ModelInsert {..} =
  interp
    False
    [sql|
    INSERT INTO users(email, password, display_name, full_name, avatar_url, is_admin)
    VALUES (#{miEmail}, #{miPassword}, #{miDisplayName}, #{miFullName}, #{miAvatarUrl}, #{miIsAdmin})
    RETURNING id
  |]

deleteUser :: Id -> Hasql.Statement () ()
deleteUser userId =
  interp
    False
    [sql|
      DELETE FROM users
      WHERE id = #{userId}
  |]

changeUserPassword :: Id -> PasswordHash Argon2 -> PasswordHash Argon2 -> Hasql.Statement () (Maybe Id)
changeUserPassword userId oldPassword newPassword =
  interp
    False
    [sql|
      UPDATE users
      SET (password = #{newPassword})
      WHERE id = #{userId} && password = #{oldPassword}
  |]
