{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.Database.Tables.User where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Password.Argon2 (Argon2, PasswordHash)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Domain.Types.EmailAddress (EmailAddress)
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
      Servant.ToHttpApiData,
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
    mPassword :: PasswordHash Argon2
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

-- | API Domain Type for @Users@.
data Domain = Domain
  { dId :: Id,
    dEmail :: EmailAddress
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance Domain)
  deriving anyclass (FromJSON, ToJSON)

toDomain :: Model -> Domain
toDomain Model {..} =
  Domain
    { dId = mId,
      dEmail = mEmail
    }

--------------------------------------------------------------------------------

getUsers :: Hasql.Statement () [Model]
getUsers =
  interp
    False
    [sql|
    SELECT id, email, password
    FROM users
  |]

getUser :: Id -> Hasql.Statement () (Maybe Model)
getUser userId =
  interp
    False
    [sql|
    SELECT id, email, password
    FROM users
    WHERE id = #{userId}
  |]

getUserByEmail :: EmailAddress -> Hasql.Statement () (Maybe Model)
getUserByEmail email =
  interp
    False
    [sql|
    SELECT id, email, password
    FROM users
    WHERE email = #{email}
  |]

getUserByCredential :: EmailAddress -> PasswordHash Argon2 -> Hasql.Statement () (Maybe Model)
getUserByCredential email password =
  interp
    False
    [sql|
      SELECT id, email, password
      FROM users
      WHERE email = #{email} AND password = #{password}
    |]

data ModelInsert = ModelInsert
  { miEmail :: EmailAddress,
    miPassword :: PasswordHash Argon2
  }
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via ModelInsert
  deriving (Display) via (RecordInstance ModelInsert)

insertUser :: ModelInsert -> Hasql.Statement () (OneRow Id)
insertUser ModelInsert {..} =
  interp
    False
    [sql|
    INSERT INTO users(email, password, created_at, updated_at)
    VALUES (#{miEmail}, #{miPassword}, NOW(), NOW())
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
      SET password = #{newPassword}, updated_at = NOW()
      WHERE id = #{userId} AND password = #{oldPassword}
      RETURNING id
  |]
