module Effects.Database.Tables.ImagesSpec where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Images qualified as UUT
import Effects.Database.Tables.User qualified as User
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

--------------------------------------------------------------------------------

imageInsertGen :: (MonadIO m, MonadGen m) => m (Text, Text)
imageInsertGen = do
  iTitle <- Gen.text (Range.linear 1 10) Gen.alphaNum
  miFilePath <- Gen.text (Range.linear 1 100) Gen.alphaNum
  pure (iTitle, miFilePath)

userInsertGen :: (MonadIO m, MonadGen m) => m User.ModelInsert
userInsertGen = do
  miEmail <- genEmail
  miDisplayName <- genDisplayName
  miFullName <- genFullName
  miPassword <- genPassword
  miAvatarUrl <- Gen.maybe $ Gen.text (Range.linear 1 10) Gen.alphaNum
  miIsAdmin <- Gen.bool
  pure User.ModelInsert {..}

imageUpdateGen :: (MonadIO m, MonadGen m) => m (Text, Text)
imageUpdateGen = do
  muTitle <- Gen.text (Range.linear 1 10) Gen.alphaNum
  muFilePath <- Gen.text (Range.linear 1 100) Gen.alphaNum
  pure (muTitle, muFilePath)
