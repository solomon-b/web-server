{-# LANGUAGE PackageImports #-}

module Effects.Database.Tables.UserSpec where

--------------------------------------------------------------------------------

import Effects.Database.Class (MonadDB (..))
import "example-server" Effects.Database.Tables.User qualified as UUT
import Hasql.Interpolate (OneRow (OneRow))
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertRight)
import Test.Gen.EmailAddress ()
import Test.Gen.Password ()
import Test.Gen.Tables.Users (userInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.User" $ do
      runs 30 . it "schema validation" $ hedgehog . prop_insertSelect

prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    userInsert <- forAllT userInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow insertedId) <- TRX.statement () (UUT.insertUser userInsert)
        selected <- TRX.statement () (UUT.getUser insertedId)
        pure (insertedId, selected)

      assert $ do
        (insertedId, mSelected) <- assertRight result
        selected <- assertJust mSelected
        UUT.miEmail userInsert === UUT.mEmail selected
        UUT.miPassword userInsert === UUT.mPassword selected
        insertedId === UUT.mId selected
