module Effects.Database.Tables.ServerSessionsSpec where

--------------------------------------------------------------------------------

import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ServerSessions qualified as UUT
import Effects.Database.Tables.User (Model (..), insertUser)
import Effects.Database.Tables.UserSpec (userInsertGen)
import Hasql.Interpolate
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (forAllT)
import Hedgehog.Range qualified as Range
import Test.Database.Expectation (isCloseTo)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property
import Test.Database.Property.Assert
import Test.Gen.Network (genNetAddrIP)
import Test.Gen.Time (futureTimeGen)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

-- TODO: Consider graphula or something for data scaffolding
spec :: Spec
spec = withTestDB $
  describe "ServerSessions" $ do
    runs 30 . it "schema validation" $ hedgehog . prop_insertSelect

prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    (userInsert, futureTime, ip, userAgent) <- forAllT $ do
      userInsert <- userInsertGen
      futureTime <- futureTimeGen
      ip <- Gen.maybe genNetAddrIP
      userAgent <- Gen.maybe $ Gen.text (Range.linear 0 20) Gen.alphaNum
      pure (userInsert, futureTime, ip, userAgent)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        TRX.condemn
        (OneRow userI) <- TRX.statement () $ insertUser userInsert

        let sessionInsert =
              UUT.ServerSessionInsert
                { ssiUserId = userI,
                  ssiIpAddress = ip,
                  ssiUserAgent = userAgent,
                  ssiExpiresAt = futureTime
                }

        (OneRow inserted) <- TRX.statement () $ UUT.insertServerSession sessionInsert

        selected <- TRX.statement () $ UUT.getServerSession $ UUT.mSessionId inserted

        pure (inserted, selected)
      assert $ do
        (inserted, mSelected) <- assertRight result
        selected <- assertJust mSelected

        UUT.mUserId inserted === UUT.mUserId selected
        UUT.mIpAddress inserted === UUT.mIpAddress selected
        UUT.mUserAgent inserted === UUT.mUserAgent selected
        UUT.mExpiresAt inserted <== isCloseTo (UUT.mExpiresAt selected)
