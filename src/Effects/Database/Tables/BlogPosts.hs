{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.Database.Tables.BlogPosts where

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

-- | Database Model for the @user@ table.
data Model = Model
  { mId :: Id,
    mAuthorId :: User.Id,
    mTitle :: Text,
    mContent :: Text,
    mPublished :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

-- | API Domain Type for @Users@.
data Domain = Domain
  { dId :: Id,
    dAuthorId :: User.Id,
    dTitle :: Text,
    dContent :: Text,
    dPublished :: Bool
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance Domain)
  deriving anyclass (FromJSON, ToJSON)

toDomain :: Model -> Domain
toDomain Model {..} =
  Domain
    { dId = mId,
      dAuthorId = mAuthorId,
      dTitle = mTitle,
      dContent = mContent,
      dPublished = mPublished
    }

--------------------------------------------------------------------------------

getBlogPosts :: Hasql.Statement () [Model]
getBlogPosts =
  interp
    False
    [sql|
    SELECT id, author_id, title, content, published
    FROM blog_posts
  |]

getBlogPost :: Id -> Hasql.Statement () (Maybe Model)
getBlogPost postId =
  interp
    False
    [sql|
    SELECT id, author_id, title, content, published
    FROM blog_posts
    WHERE id = #{postId} 
  |]

getPostsByAuthor :: Id -> Hasql.Statement () [Model]
getPostsByAuthor userId =
  interp
    False
    [sql|
    SELECT id, author_id, title, content, published
    FROM blog_posts
    WHERE author_id = #{userId} 
  |]

data ModelInsert = ModelInsert
  { miAuthorId :: User.Id,
    miTitle :: Text,
    miContent :: Text,
    miPublished :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via ModelInsert
  deriving (Display) via (RecordInstance ModelInsert)

insertBlogPost :: ModelInsert -> Hasql.Statement () (OneRow Id)
insertBlogPost ModelInsert {..} =
  interp
    False
    [sql|
    INSERT INTO blog_posts(author_id, title, content, published)
    VALUES (#{miAuthorId}, #{miTitle}, #{miContent}, #{miPublished})
    RETURNING id
  |]

deleteBlogPost :: Id -> Hasql.Statement () ()
deleteBlogPost postId =
  interp
    False
    [sql|
      DELETE FROM blog_posts
      WHERE id = #{postId}
  |]

data ModelUpdate = ModelUpdate
  { muId :: Id,
    muTitle :: Text,
    muContent :: Text,
    muPublished :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via ModelUpdate
  deriving (Display) via (RecordInstance ModelUpdate)

updateBlogPost :: ModelUpdate -> Hasql.Statement () ()
updateBlogPost ModelUpdate {..} =
  interp
    False
    [sql|
        UPDATE blog_posts
        SET title = #{muTitle},
            content = #{muContent}
            published = #{muPublished}
        WHERE id = #{muId}
  |]

publishBlogPost :: Id -> Hasql.Statement () ()
publishBlogPost postId =
  interp
    False
    [sql|
        UPDATE blog_posts
        SET published = TRUE
        WHERE id = #{postId}
  |]

unpublishBlogPost :: Id -> Hasql.Statement () ()
unpublishBlogPost postId =
  interp
    False
    [sql|
        UPDATE blog_posts
        SET published = FALSE
        WHERE id = #{postId}
  |]
