module API.User.Id.GetSpec where

--------------------------------------------------------------------------------

import API.User.Id.Get qualified as UUT
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.User qualified as User
import Hasql.Interpolate (OneRow (..))
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog.Internal.Property (forAllT, (===))
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Gen.Tables.Users (userInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (PropertyT, hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.User.Id.Get" $ do
      runs 30 . it "Gets the expected user" $ \cfg -> hedgehog $ prop_insertSelect cfg

prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    userInserts <- forAllT userInsertGen

    act $ do
      -- Insert a user
      uid <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        fmap getOneRow $ TRX.statement () $ User.insertUser userInserts

      -- Get the inserted user
      uid' <- fmap User.dId <$> traverse UUT.handler uid

      assert $ uid === uid'
