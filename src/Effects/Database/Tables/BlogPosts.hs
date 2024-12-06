{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.Database.Tables.BlogPosts where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Password.Argon2 (Argon2, PasswordHash)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
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

-- | Database Model for the @blog_posts@ table.
data Model = Model
  { mId :: Id,
    mAuthorId :: User.Id,
    mTitle :: Text,
    mContent :: Text,
    mPublished :: Bool,
    mHeroImagePath :: Maybe Text
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
    dPublished :: Bool,
    dHeroImagePath :: Maybe Text
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
      dPublished = mPublished,
      dHeroImagePath = mHeroImagePath
    }

--------------------------------------------------------------------------------

getBlogPostsWithUsers :: Hasql.Statement () [(Model, User.Model)]
getBlogPostsWithUsers =
  fmap fromRows
    <$> interp
      False
      [sql|
    SELECT
      bp.id, bp.author_id, bp.title, bp.content, bp.published, bp.hero_image_path,
      u.id, u.email, u.password, u.display_name, u.avatar_url, u.is_admin 
    FROM blog_posts AS bp
    JOIN users AS u ON u.id = bp.author_id
  |]
  where
    fromRows ::
      ( Id,
        User.Id,
        Text,
        Text,
        Bool,
        Maybe Text,
        User.Id,
        EmailAddress,
        PasswordHash Argon2,
        DisplayName,
        Maybe Text,
        Bool
      ) ->
      (Model, User.Model)
    fromRows (bId, bAuthorId, bTitle, bContent, bPublished, bHeroImagePath, uId, uEmail, uPassword, uDisplayname, uAvatarUrl, uIsAdmin) =
      ( Model bId bAuthorId bTitle bContent bPublished bHeroImagePath,
        User.Model uId uEmail uPassword uDisplayname uAvatarUrl uIsAdmin
      )

getBlogPosts :: Hasql.Statement () [Model]
getBlogPosts =
  interp
    False
    [sql|
    SELECT id, author_id, title, content, published, hero_image_path
    FROM blog_posts
  |]

getBlogPostWithUser :: Id -> Hasql.Statement () (Maybe (Model, User.Model))
getBlogPostWithUser postId =
  fmap fromRows
    <$> interp
      False
      [sql|
    SELECT
      bp.id, bp.author_id, bp.title, bp.content, bp.published, bp.hero_image_path,
      u.id, u.email, u.password, u.display_name, u.avatar_url, u.is_admin 
    FROM blog_posts AS bp
    JOIN users AS u ON u.id = bp.author_id
    WHERE bp.id = #{postId} 
  |]
  where
    fromRows ::
      ( Id,
        User.Id,
        Text,
        Text,
        Bool,
        Maybe Text,
        User.Id,
        EmailAddress,
        PasswordHash Argon2,
        DisplayName,
        Maybe Text,
        Bool
      ) ->
      (Model, User.Model)
    fromRows (bId, bAuthorId, bTitle, bContent, bPublished, bHeroImagePath, uId, uEmail, uPassword, uDisplayname, uAvatarUrl, uIsAdmin) =
      ( Model bId bAuthorId bTitle bContent bPublished bHeroImagePath,
        User.Model uId uEmail uPassword uDisplayname uAvatarUrl uIsAdmin
      )

getBlogPost :: Id -> Hasql.Statement () (Maybe Model)
getBlogPost postId =
  interp
    False
    [sql|
    SELECT id, author_id, title, content, published, hero_image_path
    FROM blog_posts
    WHERE id = #{postId} 
  |]

getPostsByAuthor :: Id -> Hasql.Statement () [Model]
getPostsByAuthor userId =
  interp
    False
    [sql|
    SELECT id, author_id, title, content, published, hero_image_path
    FROM blog_posts
    WHERE author_id = #{userId} 
  |]

data ModelInsert = ModelInsert
  { miAuthorId :: User.Id,
    miTitle :: Text,
    miContent :: Text,
    miPublished :: Bool,
    miHeroImagePath :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via ModelInsert
  deriving (Display) via (RecordInstance ModelInsert)

insertBlogPost :: ModelInsert -> Hasql.Statement () (OneRow Id)
insertBlogPost ModelInsert {..} =
  interp
    False
    [sql|
    INSERT INTO blog_posts(author_id, title, content, published, hero_image_path)
    VALUES (#{miAuthorId}, #{miTitle}, #{miContent}, #{miPublished}, #{miHeroImagePath})
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
    muPublished :: Bool,
    muHeroImagePath :: Maybe Text
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
            content = #{muContent},
            published = #{muPublished},
            hero_image_path = #{muHeroImagePath}
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
