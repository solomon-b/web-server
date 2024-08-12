{-# LANGUAGE QuasiQuotes #-}

module API.User.GetSpec where

--------------------------------------------------------------------------------

import API.User.Get qualified as UUT
import Control.Monad (void)
import Data.Foldable (traverse_)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserSpec (userInsertGen)
import Hasql.Interpolate (interp, sql)
import Hasql.Statement qualified as Hasql
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (forAllT, (===))
import Hedgehog.Range qualified as Range
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Hspec
import Test.Hspec.Hedgehog (PropertyT, hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.User.Get" $ do
      runs 30 . it "fetches the expected number of users" $ hedgehog . prop_insertSelect

prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    userInserts <- forAllT $ Gen.list (Range.linear 1 10) userInsertGen

    act $ do
      response <- do
        void $ runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
          void $ TRX.statement () deleteUsers
          traverse_ (TRX.statement () . User.insertUser) userInserts
        UUT.handler

      assert $ do
        length response === length userInserts

deleteUsers :: Hasql.Statement () ()
deleteUsers =
  interp
    False
    [sql|
      DELETE FROM users
  |]
