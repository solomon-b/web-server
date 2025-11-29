module API.User.Id.GetSpec where

--------------------------------------------------------------------------------

import API.User.Id.Get qualified as UUT
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Queries.UserWithMetadata (FullUser (..))
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Interpolate (OneRow (..))
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog.Internal.Property (forAllT, (===))
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Gen.Tables.Users (userInsertGen, userMetadataInsertGen)
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
    -- Generate metadata insert data (we'll get an ID from the user insert)
    metadataGen <- forAllT $ userMetadataInsertGen (User.Id 0) -- placeholder
    act $ do
      -- Insert a user with metadata
      uid <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        OneRow userId <- TRX.statement () $ User.insertUser userInserts
        let metadataInsert = metadataGen {UserMetadata.miUserId = userId}
        _ <- TRX.statement () $ UserMetadata.insert metadataInsert
        pure userId

      -- Get the inserted user
      uid' <- fmap fuId <$> traverse UUT.handler uid

      assert $ uid === uid'
