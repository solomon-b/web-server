{-# OPTIONS_GHC -Wno-orphans #-}

module Effects.Database.Tables.ProductsSpec where

--------------------------------------------------------------------------------

import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Products qualified as UUT
import Hasql.Interpolate
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertRight)
import Test.Gen.Tables.Products (productInsertGen, productUpdateGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.Products" $ do
      runs 30 . it "insert ; select" $ hedgehog . prop_insertSelect
      runs 30 . it "insert ; update ; select" $ hedgehog . prop_insertUpdateSelect
      runs 30 . it "insert ; delete ; select" $ hedgehog . prop_insertDeleteSelect

prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    UUT.Insert {..} <- forAllT productInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow insertedId) <- TRX.statement () (UUT.insert UUT.Insert {..})
        selected <- TRX.statement () (UUT.get insertedId)

        pure (insertedId, selected)

      assert $ do
        (insertedId, mSelected) <- assertRight result
        (product', _heroImage) <- assertJust mSelected

        iName === UUT.mName product'
        iDescription === UUT.mDescription product'
        iHeroImage === UUT.mHeroImageId product'
        iPriceCents === UUT.mPriceCents product'
        iCurrency === UUT.mCurrency product'
        iStockQuantity === UUT.mStockQuantity product'
        iPublished === UUT.mPublished product'
        insertedId === UUT.mId product'

prop_insertUpdateSelect :: TestDBConfig -> PropertyT IO ()
prop_insertUpdateSelect cfg = do
  arrange (bracketConn cfg) $ do
    UUT.Insert {..} <- forAllT productInsertGen
    mkUpdate <- forAllT productUpdateGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow insertedId) <- TRX.statement () (UUT.insert UUT.Insert {..})
        let UUT.Model {..} = mkUpdate insertedId
        () <- TRX.statement () (UUT.update UUT.Model {UUT.mId = insertedId, ..})
        selected <- TRX.statement () (UUT.get insertedId)
        pure (insertedId, selected)

      assert $ do
        (insertedId, mSelected) <- assertRight result
        (product', _heroImage) <- assertJust mSelected
        let UUT.Model {..} = mkUpdate insertedId

        mName === UUT.mName product'
        mDescription === UUT.mDescription product'
        mHeroImageId === UUT.mHeroImageId product'
        mPriceCents === UUT.mPriceCents product'
        mCurrency === UUT.mCurrency product'
        mStockQuantity === UUT.mStockQuantity product'
        mPublished === UUT.mPublished product'
        insertedId === UUT.mId product'

prop_insertDeleteSelect :: TestDBConfig -> PropertyT IO ()
prop_insertDeleteSelect cfg = do
  arrange (bracketConn cfg) $ do
    UUT.Insert {..} <- forAllT productInsertGen
    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow insertedId) <- TRX.statement () (UUT.insert UUT.Insert {..})
        inserted <- TRX.statement () (UUT.get insertedId)
        () <- TRX.statement () (UUT.delete insertedId)
        selected <- TRX.statement () (UUT.get insertedId)

        pure (inserted, selected)

      assert $ do
        (inserted, selectedAfterDelete) <- assertRight result
        (_product, _heroImage) <- assertJust inserted

        selectedAfterDelete === Nothing
