module Effects.Database.Tables.BlogPostsSpec where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.BlogPosts qualified as UUT
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
import Test.Gen.Password
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.BlogPosts" $ do
      runs 30 . it "schema validation" $ hedgehog . prop_insertSelect

prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    userInsert <- forAllT userInsertGen
    blogPostInsert <- forAllT blogPostInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow userId) <- TRX.statement () (User.insertUser userInsert)
        (OneRow insertedId) <- TRX.statement () (UUT.insertBlogPost (blogPostInsert {UUT.miAuthorId = userId}))
        selected <- TRX.statement () (UUT.getBlogPost insertedId)
        pure (userId, insertedId, selected)

      assert $ do
        (userId, insertedId, mSelected) <- assertRight result
        selected <- assertJust mSelected
        UUT.miTitle blogPostInsert === UUT.mTitle selected
        UUT.miContent blogPostInsert === UUT.mContent selected
        UUT.miPublished blogPostInsert === UUT.mPublished selected
        UUT.miHeroImagePath blogPostInsert === UUT.mHeroImagePath selected
        userId === UUT.mAuthorId selected
        insertedId === UUT.mId selected

blogPostInsertGen :: (MonadIO m, MonadGen m) => m UUT.ModelInsert
blogPostInsertGen = do
  -- TODO: Figure out how to Gen a function.
  let miAuthorId = User.Id undefined
  miTitle <- Gen.text (Range.linear 1 10) Gen.alphaNum
  miContent <- Gen.text (Range.linear 1 10) Gen.alphaNum
  miPublished <- Gen.bool
  miHeroImagePath <- Gen.maybe $ Gen.text (Range.linear 1 100) Gen.alphaNum
  pure UUT.ModelInsert {..}

userInsertGen :: (MonadIO m, MonadGen m) => m User.ModelInsert
userInsertGen = do
  miEmail <- genEmail
  miDisplayName <- genDisplayName
  miPassword <- genPassword
  miAvatarUrl <- Gen.maybe $ Gen.text (Range.linear 1 10) Gen.alphaNum
  miIsAdmin <- Gen.bool
  pure User.ModelInsert {..}
