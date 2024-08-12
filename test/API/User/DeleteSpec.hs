{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module API.User.DeleteSpec where

--------------------------------------------------------------------------------

import API.User.Delete qualified as UUT
import Auth (Authz)
import Data.Foldable (traverse_)
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserSpec (userInsertGen)
import Hasql.Interpolate (OneRow (..), interp, sql)
import Hasql.Statement qualified as Hasql
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog.Internal.Property (forAllT, (===))
import Test.Database.Monad (TestDBConfig, bracketConn, withAuth, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Hspec
import Test.Hspec.Hedgehog (PropertyT, hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    withAuth $
      describe "API.User.Delete" $ do
        runs 30 . it "deletes the expected user" $ \(auth, cfg) -> hedgehog $ prop_insertSelect auth cfg

prop_insertSelect :: Authz -> TestDBConfig -> PropertyT IO ()
prop_insertSelect auth cfg = do
  arrange (bracketConn cfg) $ do
    userInserts <- forAllT userInsertGen

    act $ do
      -- Insert a user
      userToDelete <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        uid <- TRX.statement () $ User.insertUser userInserts
        user <- fmap (fromMaybe $ error "oops") $ TRX.statement () $ User.getUser $ getOneRow uid
        pure (uid, user)

      -- Fetch all users
      allUsers <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Read $ TRX.statement () User.getUsers

      -- Delete the inserted user
      traverse_ (UUT.handler auth . getOneRow . fst) userToDelete

      -- Fetch remaining users
      remUsers <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Read $ TRX.statement () User.getUsers

      -- The diff should be the deleted user
      let diff = liftA2 (\\) allUsers remUsers
      assert $ diff === fmap (pure . snd) userToDelete

deleteUsers :: Hasql.Statement () ()
deleteUsers =
  interp
    False
    [sql|
      DELETE FROM users
      WHERE emailAddressl /= 'user@host.com'
  |]
