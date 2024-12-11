{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.Database.Tables.Images where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Effects.Database.Tables.User qualified as User
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

-- | Database Model for the @images@ table.
data Model = Model
  { mId :: Id,
    mUserId :: User.Id,
    mTitle :: Text,
    mFilePath :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

-- | API Domain Type for @Users@.
data Domain = Domain
  { dId :: Id,
    dUserId :: User.Id,
    dTitle :: Text,
    dFilePath :: Text
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance Domain)
  deriving anyclass (FromJSON, ToJSON)

toDomain :: Model -> Domain
toDomain Model {..} =
  Domain
    { dId = mId,
      dUserId = mUserId,
      dTitle = mTitle,
      dFilePath = mFilePath
    }

fromDomain :: Domain -> Model
fromDomain Domain {..} =
  Model
    { mId = dId,
      mUserId = dUserId,
      mTitle = dTitle,
      mFilePath = dFilePath
    }

--------------------------------------------------------------------------------

getImages :: Hasql.Statement () [Model]
getImages =
  interp
    False
    [sql|
    SELECT id, user_id, title, file_path
    FROM images
  |]

getImage :: Id -> Hasql.Statement () (Maybe Model)
getImage imageId =
  interp
    False
    [sql|
    SELECT id, user_id, title, file_path
    FROM images
    WHERE id = #{imageId} 
  |]

getPostsByAuthor :: Id -> Hasql.Statement () [Model]
getPostsByAuthor userId =
  interp
    False
    [sql|
    SELECT id, user_id, title, file_path
    FROM images
    WHERE user_id = #{userId} 
  |]

data ModelInsert = ModelInsert
  { miUserId :: User.Id,
    iTitle :: Text,
    miFilePath :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via ModelInsert
  deriving (Display) via (RecordInstance ModelInsert)

insertImage :: ModelInsert -> Hasql.Statement () (OneRow Id)
insertImage ModelInsert {..} =
  interp
    False
    [sql|
    INSERT INTO images(user_id, title, file_path)
    VALUES (#{miUserId}, #{iTitle}, #{miFilePath})
    RETURNING id
  |]

deleteImage :: Id -> Hasql.Statement () ()
deleteImage postId =
  interp
    False
    [sql|
      DELETE FROM images
      WHERE id = #{postId}
  |]

data ModelUpdate = ModelUpdate
  { muId :: Id,
    muTitle :: Text,
    muFilePath :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via ModelUpdate
  deriving (Display) via (RecordInstance ModelUpdate)

updateImage :: ModelUpdate -> Hasql.Statement () ()
updateImage ModelUpdate {..} =
  interp
    False
    [sql|
        UPDATE images
        SET title = #{muTitle},
            file_path = #{muFilePath}
        WHERE id = #{muId}
  |]
