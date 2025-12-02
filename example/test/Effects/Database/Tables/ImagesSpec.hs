{-# LANGUAGE PackageImports #-}

module Effects.Database.Tables.ImagesSpec where

--------------------------------------------------------------------------------

import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Images qualified as UUT
import "example-server" Effects.Database.Tables.User qualified as User
import Hasql.Interpolate (OneRow (OneRow))
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertRight)
import Test.Gen.Tables.Images (imageInsertGen, imageUpdateGen)
import Test.Gen.Tables.Users (userInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.Images" $ do
      runs 30 . it "insert ; select" $ hedgehog . prop_insertSelect
      runs 30 . it "insert ; update ; select" $ hedgehog . prop_insertUpdateSelect

--------------------------------------------------------------------------------

prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    userInsert <- forAllT userInsertGen
    (iTitle, miFilePath) <- forAllT imageInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow userId) <- TRX.statement () (User.insertUser userInsert)
        (OneRow insertedId) <- TRX.statement () (UUT.insertImage UUT.ModelInsert {UUT.miUserId = userId, ..})
        selected <- TRX.statement () (UUT.getImage insertedId)
        pure (userId, insertedId, selected)

      assert $ do
        (userId, insertedId, mSelected) <- assertRight result
        selected <- assertJust mSelected
        iTitle === UUT.mTitle selected
        miFilePath === UUT.mFilePath selected
        userId === UUT.mUserId selected
        insertedId === UUT.mId selected

prop_insertUpdateSelect :: TestDBConfig -> PropertyT IO ()
prop_insertUpdateSelect cfg = do
  arrange (bracketConn cfg) $ do
    userInsert <- forAllT userInsertGen
    (iTitle, miFilePath) <- forAllT imageInsertGen
    (muTitle, muFilePath) <- forAllT imageUpdateGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow userId) <- TRX.statement () (User.insertUser userInsert)
        (OneRow insertedId) <- TRX.statement () (UUT.insertImage UUT.ModelInsert {UUT.miUserId = userId, ..})
        () <- TRX.statement () (UUT.updateImage UUT.ModelUpdate {UUT.muId = insertedId, ..})
        selected <- TRX.statement () (UUT.getImage insertedId)
        pure (userId, insertedId, selected)

      assert $ do
        (userId, insertedId, mSelected) <- assertRight result
        selected <- assertJust mSelected
        muTitle === UUT.mTitle selected
        muFilePath === UUT.mFilePath selected
        userId === UUT.mUserId selected
        insertedId === UUT.mId selected
