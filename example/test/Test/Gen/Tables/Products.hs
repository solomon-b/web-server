{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Gen.Tables.Products where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO)
import Domain.Types.Amount (Amount (..))
import Domain.Types.ProductName (mkProductNameUnsafe)
import Effects.Database.Tables.Products qualified as UUT
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------

productInsertGen :: (MonadIO m, MonadGen m) => m UUT.Insert
productInsertGen = do
  iName <- mkProductNameUnsafe <$> Gen.text (Range.linear 1 10) Gen.alphaNum
  iDescription <- Gen.text (Range.linear 1 10) Gen.alphaNum
  let iHeroImage = Nothing
  iPriceCents <- Amount <$> Gen.int64 (Range.linear 1 10)
  iCurrency <- Gen.text (Range.linear 1 10) Gen.alphaNum
  iStockQuantity <- Gen.int64 (Range.linear 1 10)
  iPublished <- Gen.bool

  pure UUT.Insert {..}

instance Show (UUT.Id -> UUT.Model) where
  show _ = "Id -> Model"

productUpdateGen :: (MonadIO m, MonadGen m) => m (UUT.Id -> UUT.Model)
productUpdateGen = do
  mName <- mkProductNameUnsafe <$> Gen.text (Range.linear 1 10) Gen.alphaNum
  mDescription <- Gen.text (Range.linear 1 10) Gen.alphaNum
  let mHeroImageId = Nothing
  mPriceCents <- Amount <$> Gen.int64 (Range.linear 1 10)
  mCurrency <- Gen.text (Range.linear 1 10) Gen.alphaNum
  mStockQuantity <- Gen.int64 (Range.linear 1 10)
  mPublished <- Gen.bool

  pure $ \mId -> UUT.Model {..}
