module Effects.Database.Tables.MailingListSpec where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))
import Data.List ((\\))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.MailingList qualified as UUT
import Hasql.Interpolate
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (MonadGen (..), PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Monad
import Test.Database.Property
import Test.Database.Property.Assert
import Test.Gen.EmailAddress
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.MailingList" $ do
      runs 30 . it "schema validation" $ hedgehog . prop_insertSelect

prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    userInsert <- forAllT userInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        oldSelected <- TRX.statement () UUT.getEmailListEntries
        (OneRow inserted) <- TRX.statement () (UUT.insertEmailAddress userInsert)
        newSelected <- TRX.statement () UUT.getEmailListEntries
        pure (inserted, oldSelected, newSelected)

      assert $ do
        (inserted, oldSelected, newSelected) <- assertRight result
        length newSelected - length oldSelected === 1
        [inserted] === (newSelected \\ oldSelected)

userInsertGen :: (MonadIO m, MonadGen m) => m UUT.ModelInsert
userInsertGen = do
  miEmail <- genEmail
  pure UUT.ModelInsert {..}
