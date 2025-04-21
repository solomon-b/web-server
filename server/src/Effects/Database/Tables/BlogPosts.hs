{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.Database.Tables.BlogPosts where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Password.Argon2 (Argon2, PasswordHash)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Data.Time (UTCTime)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import Effects.Database.Tables.Images qualified as Images
import Effects.Database.Tables.User qualified as User
import GHC.Generics
import Hasql.Interpolate (DecodeRow, DecodeValue, EncodeRow, EncodeValue, OneRow, interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Password ()
import OrphanInstances.UTCTime ()
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

-- | Database Model for the @blog_posts@ table.
data Model = Model
  { mId :: Id,
    mAuthorId :: User.Id,
    mTitle :: Subject,
    mContent :: Body,
    mPublishedAt :: Maybe UTCTime,
    mHeroImageId :: Maybe Images.Id
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

-- | Domain Type (business object) for @Blog Posts@.
data Domain = Domain
  { dId :: Id,
    dAuthorId :: User.Id,
    dTitle :: Subject,
    dContent :: Body,
    dPublishedAt :: Maybe UTCTime,
    dHeroImage :: Maybe Images.Domain
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance Domain)
  deriving anyclass (FromJSON, ToJSON)

toDomain :: (Model, Maybe Images.Model) -> Domain
toDomain (Model {..}, image) =
  Domain
    { dId = mId,
      dAuthorId = mAuthorId,
      dTitle = mTitle,
      dContent = mContent,
      dPublishedAt = mPublishedAt,
      dHeroImage = fmap Images.toDomain image
    }

fromDomain :: Domain -> (Model, Maybe Images.Model)
fromDomain Domain {..} =
  let mHeroImage = fmap Images.fromDomain dHeroImage
   in ( Model
          { mId = dId,
            mAuthorId = dAuthorId,
            mTitle = dTitle,
            mContent = dContent,
            mPublishedAt = dPublishedAt,
            mHeroImageId = fmap Images.mId mHeroImage
          },
        mHeroImage
      )

--------------------------------------------------------------------------------

getBlogPostsWithUsers :: Hasql.Statement () [(Model, User.Model)]
getBlogPostsWithUsers =
  fmap fromRows
    <$> interp
      False
      [sql|
    SELECT
      bp.id, bp.author_id, bp.title, bp.content, bp.published_at, bp.hero_image_id,
      u.id, u.email, u.password, u.display_name, u.full_name, u.avatar_url, u.is_admin 
    FROM blog_posts AS bp
    JOIN users AS u ON u.id = bp.author_id
  |]
  where
    fromRows ::
      ( Id,
        User.Id,
        Subject,
        Body,
        Maybe UTCTime,
        Maybe Images.Id,
        User.Id,
        EmailAddress,
        PasswordHash Argon2,
        DisplayName,
        FullName,
        Maybe Text,
        Bool
      ) ->
      (Model, User.Model)
    fromRows (bId, bAuthorId, bTitle, bContent, bPublishedAt, bHeroImageId, uId, uEmail, uPassword, uDisplayname, uFullName, uAvatarUrl, uIsAdmin) =
      ( Model bId bAuthorId bTitle bContent bPublishedAt bHeroImageId,
        User.Model uId uEmail uPassword uDisplayname uFullName uAvatarUrl uIsAdmin
      )

getPublishedBlogPosts :: Hasql.Statement () [(Model, Maybe Images.Model)]
getPublishedBlogPosts =
  fmap fromRows
    <$> interp
      False
      [sql|
    SELECT
      bp.id, bp.author_id, bp.title, bp.content, bp.published_at, bp.hero_image_id,
      i.user_id, i.title, i.file_path
    FROM blog_posts AS bp
    LEFT JOIN images AS i ON (i.id = bp.hero_image_id)
    WHERE
      bp.published_at IS NOT NULL
  |]
  where
    fromRows ::
      ( Id,
        User.Id,
        Subject,
        Body,
        Maybe UTCTime,
        Maybe Images.Id,
        Maybe User.Id,
        Maybe Text,
        Maybe Text
      ) ->
      (Model, Maybe Images.Model)
    fromRows (bId, bAuthorId, bTitle, bContent, bPublishedAt, bHeroImageId, iUserId, iTitle, iFilePath) =
      ( Model bId bAuthorId bTitle bContent bPublishedAt bHeroImageId,
        Images.Model <$> bHeroImageId <*> iUserId <*> iTitle <*> iFilePath
      )

getBlogPosts :: Hasql.Statement () [(Model, Maybe Images.Model)]
getBlogPosts =
  fmap fromRows
    <$> interp
      False
      [sql|
    SELECT
      bp.id, bp.author_id, bp.title, bp.content, bp.published_at, bp.hero_image_id,
      i.user_id, i.title, i.file_path 
    FROM blog_posts AS bp
    LEFT JOIN images AS i ON (i.id = bp.hero_image_id)
  |]
  where
    fromRows ::
      ( Id,
        User.Id,
        Subject,
        Body,
        Maybe UTCTime,
        Maybe Images.Id,
        Maybe User.Id,
        Maybe Text,
        Maybe Text
      ) ->
      (Model, Maybe Images.Model)
    fromRows (bId, bAuthorId, bTitle, bContent, bPublishedAt, bHeroImageId, iUserId, iTitle, iFilePath) =
      ( Model bId bAuthorId bTitle bContent bPublishedAt bHeroImageId,
        Images.Model <$> bHeroImageId <*> iUserId <*> iTitle <*> iFilePath
      )

getBlogPostWithUser :: Id -> Hasql.Statement () (Maybe (Model, User.Model))
getBlogPostWithUser postId =
  fmap fromRows
    <$> interp
      False
      [sql|
    SELECT
      bp.id, bp.author_id, bp.title, bp.content, bp.published_at, bp.hero_image_id,
      u.id, u.email, u.password, u.display_name, u.full_name, u.avatar_url, u.is_admin 
    FROM blog_posts AS bp
    JOIN users AS u ON u.id = bp.author_id
    WHERE bp.id = #{postId} 
  |]
  where
    fromRows ::
      ( Id,
        User.Id,
        Subject,
        Body,
        Maybe UTCTime,
        Maybe Images.Id,
        User.Id,
        EmailAddress,
        PasswordHash Argon2,
        DisplayName,
        FullName,
        Maybe Text,
        Bool
      ) ->
      (Model, User.Model)
    fromRows (bId, bAuthorId, bTitle, bContent, bPublishedAt, bHeroImageId, uId, uEmail, uPassword, uDisplayname, uFullName, uAvatarUrl, uIsAdmin) =
      ( Model bId bAuthorId bTitle bContent bPublishedAt bHeroImageId,
        User.Model uId uEmail uPassword uDisplayname uFullName uAvatarUrl uIsAdmin
      )

getBlogPost :: Id -> Hasql.Statement () (Maybe (Model, Maybe Images.Model))
getBlogPost postId =
  fmap fromRows
    <$> interp
      False
      [sql|
    SELECT
      bp.id, bp.author_id, bp.title, bp.content, bp.published_at, bp.hero_image_id,
      i.user_id, i.title, i.file_path 
    FROM blog_posts AS bp
    LEFT JOIN images AS i ON i.id = bp.hero_image_id
    WHERE bp.id = #{postId} 
  |]
  where
    fromRows ::
      ( Id,
        User.Id,
        Subject,
        Body,
        Maybe UTCTime,
        Maybe Images.Id,
        Maybe User.Id,
        Maybe Text,
        Maybe Text
      ) ->
      (Model, Maybe Images.Model)
    fromRows (bId, bAuthorId, bTitle, bContent, bPublishedAt, bHeroImageId, iUserId, iTitle, iFilePath) =
      ( Model bId bAuthorId bTitle bContent bPublishedAt bHeroImageId,
        Images.Model <$> bHeroImageId <*> iUserId <*> iTitle <*> iFilePath
      )

getPostsByAuthor :: Id -> Hasql.Statement () [Model]
getPostsByAuthor userId =
  interp
    False
    [sql|
    SELECT id, author_id, title, content, published_at, hero_image_id
    FROM blog_posts
    WHERE author_id = #{userId} 
  |]

newtype Subject = Subject {getSubject :: Text}
  deriving stock (Show, Eq)
  deriving newtype (FromJSON, ToJSON, Servant.FromHttpApiData, Servant.ToHttpApiData, DecodeValue, EncodeValue, Display, Semigroup, Monoid, IsString)

newtype Body = Body {getBody :: Text}
  deriving stock (Show, Eq)
  deriving newtype (FromJSON, ToJSON, Servant.FromHttpApiData, Servant.ToHttpApiData, DecodeValue, EncodeValue, Display, Semigroup, Monoid, IsString)

data Insert = Insert
  { iAuthorId :: User.Id,
    iTitle :: Subject,
    iContent :: Body,
    iPublished :: Bool,
    iHeroImageId :: Maybe Images.Id
  }
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via Insert
  deriving (Display) via (RecordInstance Insert)

insertBlogPost :: Insert -> Hasql.Statement () (OneRow Id)
insertBlogPost Insert {..} =
  interp
    False
    [sql|
    INSERT INTO blog_posts(author_id, title, content, published_at, hero_image_id)
    VALUES (#{iAuthorId}, #{iTitle}, #{iContent}, CASE WHEN #{iPublished} THEN NOW() ELSE NULL END, #{iHeroImageId})
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

updateBlogPost :: Model -> Hasql.Statement () ()
updateBlogPost Model {..} =
  interp
    False
    [sql|
        UPDATE blog_posts
        SET title = #{mTitle},
            content = #{mContent},
            published_at = #{mPublishedAt},
            hero_image_id = #{mHeroImageId}
        WHERE id = #{mId}
  |]

publishBlogPost :: Id -> Hasql.Statement () ()
publishBlogPost postId =
  interp
    False
    [sql|
        UPDATE blog_posts
        SET published_at = now()
        WHERE id = #{postId}
  |]

unpublishBlogPost :: Id -> Hasql.Statement () ()
unpublishBlogPost postId =
  interp
    False
    [sql|
        UPDATE blog_posts
        SET published = null
        WHERE id = #{postId}
  |]
