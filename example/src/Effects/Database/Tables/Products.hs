{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.Products where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Domain.Types.Amount (Amount)
import Domain.Types.ProductName (ProductName)
import Effects.Database.Tables.Images qualified as Images
import Effects.Database.Tables.User qualified as User
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue, EncodeRow, EncodeValue, OneRow, interp, sql)
import Hasql.Statement qualified as Hasql
import Servant qualified

--------------------------------------------------------------------------------

newtype Id = Id Int64
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, DecodeRow)
  deriving newtype
    ( Show,
      Eq,
      Ord,
      Num,
      Servant.FromHttpApiData,
      Servant.ToHttpApiData,
      Display,
      DecodeValue,
      EncodeValue
    )

-- | Database Model for the `mailing_list` table.
data Model = Model
  { mId :: Id,
    mName :: ProductName,
    mDescription :: Text,
    mHeroImageId :: Maybe Images.Id,
    mPriceCents :: Amount,
    mCurrency :: Text,
    mStockQuantity :: Int64,
    mPublished :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

-- | Domain Type (business object) for @Products@.
data Domain = Domain
  { dId :: Id,
    dName :: ProductName,
    dDescription :: Text,
    dHeroImage :: Maybe Images.Domain,
    dPriceCents :: Amount,
    dCurrency :: Text,
    dStockQuantity :: Int64,
    dPublished :: Bool
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance Domain)
  deriving anyclass (FromJSON, ToJSON)

toDomain :: (Model, Maybe Images.Model) -> Domain
toDomain (Model {..}, image) =
  Domain
    { dId = mId,
      dName = mName,
      dDescription = mDescription,
      dHeroImage = Images.toDomain <$> image,
      dPriceCents = mPriceCents,
      dCurrency = mCurrency,
      dStockQuantity = mStockQuantity,
      dPublished = mPublished
    }

--------------------------------------------------------------------------------

gets :: Hasql.Statement () [(Model, Maybe Images.Model)]
gets =
  fmap fromRow
    <$> interp
      False
      [sql|
    SELECT
      products.id,
      products.name,
      products.description,
      products.hero_image_id,
      products.price_cents,
      products.currency,
      products.stock_quantity,
      products.published,

      images.user_id,
      images.title,
      images.file_path
    FROM
      products
    LEFT JOIN
      images
    ON
      products.hero_image_id = images.id
  |]
  where
    fromRow ::
      ( Id,
        ProductName,
        Text,
        Maybe Images.Id,
        Amount,
        Text,
        Int64,
        Bool,
        Maybe User.Id,
        Maybe Text,
        Maybe Text
      ) ->
      (Model, Maybe Images.Model)
    fromRow (pId, pName, pDescription, pHeroImageId, pPriceCents, pCurrency, pStockQuantity, pPublished, iUserId, iTitle, iFilePath) =
      ( Model pId pName pDescription pHeroImageId pPriceCents pCurrency pStockQuantity pPublished,
        Images.Model <$> pHeroImageId <*> iUserId <*> iTitle <*> iFilePath
      )

get :: Id -> Hasql.Statement () (Maybe (Model, Maybe Images.Model))
get productId =
  fmap fromRow
    <$> interp
      False
      [sql|
    SELECT
      products.id,
      products.name,
      products.description,
      products.hero_image_id,
      products.price_cents,
      products.currency,
      products.stock_quantity,
      products.published,

      images.user_id,
      images.title,
      images.file_path
    FROM
      products
    LEFT JOIN
      images
    ON products.hero_image_id = images.id
    WHERE products.id = #{productId}
  |]
  where
    fromRow ::
      ( Id,
        ProductName,
        Text,
        Maybe Images.Id,
        Amount,
        Text,
        Int64,
        Bool,
        Maybe User.Id,
        Maybe Text,
        Maybe Text
      ) ->
      (Model, Maybe Images.Model)
    fromRow (pId, pName, pDescription, pHeroImageId, pPriceCents, pCurrency, pStockQuantity, pPublished, iUserId, iTitle, iFilePath) =
      ( Model pId pName pDescription pHeroImageId pPriceCents pCurrency pStockQuantity pPublished,
        Images.Model <$> pHeroImageId <*> iUserId <*> iTitle <*> iFilePath
      )

data Insert = Insert
  { iName :: ProductName,
    iDescription :: Text,
    iHeroImage :: Maybe Images.Id,
    iPriceCents :: Amount,
    iCurrency :: Text,
    iStockQuantity :: Int64,
    iPublished :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via Insert
  deriving (Display) via (RecordInstance Insert)

insert :: Insert -> Hasql.Statement () (OneRow Id)
insert Insert {..} =
  interp
    False
    [sql|
    INSERT INTO products(name, description, hero_image_id, price_cents, currency, stock_quantity, published)
    VALUES (#{iName}, #{iDescription}, #{iHeroImage}, #{iPriceCents}, #{iCurrency}, #{iStockQuantity}, #{iPublished})
    RETURNING id
  |]

delete :: Id -> Hasql.Statement () ()
delete productId =
  interp
    False
    [sql|
      DELETE FROM products
      WHERE id = #{productId}
  |]

update :: Model -> Hasql.Statement () ()
update Model {..} =
  interp
    False
    [sql|
        UPDATE products
        SET name = #{mName},
            description = #{mDescription},
            hero_image_id = #{mHeroImageId},
            price_cents = #{mPriceCents},
            currency = #{mCurrency},
            stock_quantity = #{mStockQuantity},
            published = #{mPublished}
        WHERE id = #{mId}
  |]

togglePublished :: Id -> Hasql.Statement () ()
togglePublished productId =
  interp
    False
    [sql|
        UPDATE products
        SET published =
            CASE
                WHEN published IS FALSE THEN TRUE
                ELSE FALSE
            END
        WHERE id = #{productId}
  |]
