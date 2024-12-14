module Effects.Database.Tables.UserSpec where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.User qualified as UUT
import Hasql.Interpolate
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (MonadGen (..), PropertyT, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (forAllT)
import Hedgehog.Range qualified as Range
import Test.Database.Monad
import Test.Database.Property
import Test.Database.Property.Assert
import Test.Gen.DisplayName
import Test.Gen.EmailAddress
import Test.Gen.FullName
import Test.Gen.Password
import Test.Hspec
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
        UUT.miDisplayName userInsert === UUT.mDisplayName selected
        UUT.miAvatarUrl userInsert === UUT.mAvatarUrl selected
        UUT.miIsAdmin userInsert === UUT.mIsAdmin selected
        insertedId === UUT.mId selected

userInsertGen :: (MonadIO m, MonadGen m) => m UUT.ModelInsert
userInsertGen = do
  miEmail <- genEmail
  miDisplayName <- genDisplayName
  miFullName <- genFullName
  miPassword <- genPassword
  miAvatarUrl <- Gen.maybe $ Gen.text (Range.linear 1 10) Gen.alphaNum
  miIsAdmin <- Gen.bool
  pure UUT.ModelInsert {..}
