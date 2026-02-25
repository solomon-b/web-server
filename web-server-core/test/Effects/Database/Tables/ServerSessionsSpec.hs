module Effects.Database.Tables.ServerSessionsSpec where

--------------------------------------------------------------------------------

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ServerSessions qualified as UUT
import Effects.Database.Tables.User (insertUser)
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
import Test.Gen.Tables.Users
import Test.Gen.Time (futureTimeGen)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

-- TODO: Consider graphula or something for data scaffolding
spec :: Spec
spec = withTestDB $
  describe "Effects.Database.Tables.ServerSessions" $ do
    runs 30 . it "schema validation" $ hedgehog . prop_insertSelect
    runs 10 . it "touchSession updates last_activity_at" $ hedgehog . prop_touchSession
    runs 10 . it "expireSession makes session invisible to getServerSession" $ hedgehog . prop_expireSession
    runs 10 . it "last_activity_at round-trips correctly" $ hedgehog . prop_lastActivityAtRoundTrip

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

prop_touchSession :: TestDBConfig -> PropertyT IO ()
prop_touchSession cfg = do
  arrange (bracketConn cfg) $ do
    (userInsert, futureTime) <- forAllT $ do
      userInsert <- userInsertGen
      futureTime <- futureTimeGen
      pure (userInsert, futureTime)

    act $ do
      -- First transaction: insert session (no condemn so it commits)
      insertResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow userI) <- TRX.statement () $ insertUser userInsert

        let sessionInsert =
              UUT.ServerSessionInsert
                { ssiUserId = userI,
                  ssiIpAddress = Nothing,
                  ssiUserAgent = Nothing,
                  ssiExpiresAt = futureTime
                }

        (OneRow inserted) <- TRX.statement () $ UUT.insertServerSession sessionInsert
        pure inserted

      -- Small delay so NOW() advances between transactions
      liftIO $ threadDelay 100000

      -- Second transaction: touch and re-read
      let sId = either (error . show) UUT.mSessionId insertResult
      let activityBefore = either (error . show) UUT.mLastActivityAt insertResult
      touchResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        TRX.statement () $ UUT.touchSession sId
        TRX.statement () $ UUT.getServerSession sId

      assert $ do
        _ <- assertRight insertResult
        mUpdated <- assertRight touchResult
        updated <- assertJust mUpdated
        let activityAfter = UUT.mLastActivityAt updated
        activityAfter =>= activityBefore

prop_expireSession :: TestDBConfig -> PropertyT IO ()
prop_expireSession cfg = do
  arrange (bracketConn cfg) $ do
    (userInsert, futureTime) <- forAllT $ do
      userInsert <- userInsertGen
      futureTime <- futureTimeGen
      pure (userInsert, futureTime)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        TRX.condemn
        (OneRow userI) <- TRX.statement () $ insertUser userInsert

        let sessionInsert =
              UUT.ServerSessionInsert
                { ssiUserId = userI,
                  ssiIpAddress = Nothing,
                  ssiUserAgent = Nothing,
                  ssiExpiresAt = futureTime
                }

        (OneRow inserted) <- TRX.statement () $ UUT.insertServerSession sessionInsert
        let sId = UUT.mSessionId inserted

        TRX.statement () $ UUT.expireSession sId
        mSelected <- TRX.statement () $ UUT.getServerSession sId
        pure mSelected

      assert $ do
        mSelected <- assertRight result
        mSelected === Nothing

prop_lastActivityAtRoundTrip :: TestDBConfig -> PropertyT IO ()
prop_lastActivityAtRoundTrip cfg = do
  arrange (bracketConn cfg) $ do
    (userInsert, futureTime) <- forAllT $ do
      userInsert <- userInsertGen
      futureTime <- futureTimeGen
      pure (userInsert, futureTime)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        TRX.condemn
        (OneRow userI) <- TRX.statement () $ insertUser userInsert

        let sessionInsert =
              UUT.ServerSessionInsert
                { ssiUserId = userI,
                  ssiIpAddress = Nothing,
                  ssiUserAgent = Nothing,
                  ssiExpiresAt = futureTime
                }

        (OneRow inserted) <- TRX.statement () $ UUT.insertServerSession sessionInsert
        mSelected <- TRX.statement () $ UUT.getServerSession (UUT.mSessionId inserted)
        pure (inserted, mSelected)

      assert $ do
        (inserted, mSelected) <- assertRight result
        selected <- assertJust mSelected
        UUT.mLastActivityAt inserted <== isCloseTo (UUT.mLastActivityAt selected)
