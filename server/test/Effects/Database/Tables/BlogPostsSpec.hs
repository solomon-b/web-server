module Effects.Database.Tables.BlogPostsSpec where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
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
      runs 30 . it "insert ; select" $ hedgehog . prop_insertSelect
      runs 30 . it "insert ; update ; select" $ hedgehog . prop_insertUpdateSelect

prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    userInsert <- forAllT userInsertGen
    (miTitle, miContent, miPublished, miHeroImagePath) <- forAllT blogPostInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow userId) <- TRX.statement () (User.insertUser userInsert)
        (OneRow insertedId) <- TRX.statement () (UUT.insertBlogPost UUT.ModelInsert {UUT.miAuthorId = userId, ..})
        selected <- TRX.statement () (UUT.getBlogPost insertedId)
        pure (userId, insertedId, selected)

      assert $ do
        (userId, insertedId, mSelected) <- assertRight result
        selected <- assertJust mSelected
        miTitle === UUT.mTitle selected
        miContent === UUT.mContent selected
        miPublished === UUT.mPublished selected
        miHeroImagePath === UUT.mHeroImagePath selected
        userId === UUT.mAuthorId selected
        insertedId === UUT.mId selected

prop_insertUpdateSelect :: TestDBConfig -> PropertyT IO ()
prop_insertUpdateSelect cfg = do
  arrange (bracketConn cfg) $ do
    userInsert <- forAllT userInsertGen
    (miTitle, miContent, miPublished, miHeroImagePath) <- forAllT blogPostInsertGen
    (muTitle, muContent, muPublished, muHeroImagePath) <- forAllT blogPostUpdateGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow userId) <- TRX.statement () (User.insertUser userInsert)
        (OneRow insertedId) <- TRX.statement () (UUT.insertBlogPost UUT.ModelInsert {UUT.miAuthorId = userId, ..})
        () <- TRX.statement () (UUT.updateBlogPost UUT.ModelUpdate {UUT.muId = insertedId, ..})
        selected <- TRX.statement () (UUT.getBlogPost insertedId)
        pure (userId, insertedId, selected)

      assert $ do
        (userId, insertedId, mSelected) <- assertRight result
        selected <- assertJust mSelected
        muTitle === UUT.mTitle selected
        muContent === UUT.mContent selected
        muPublished === UUT.mPublished selected
        muHeroImagePath === UUT.mHeroImagePath selected
        userId === UUT.mAuthorId selected
        insertedId === UUT.mId selected

--------------------------------------------------------------------------------

blogPostInsertGen :: (MonadIO m, MonadGen m) => m (Text, Text, Bool, Maybe Text)
blogPostInsertGen = do
  miTitle <- Gen.text (Range.linear 1 10) Gen.alphaNum
  miContent <- Gen.text (Range.linear 1 10) Gen.alphaNum
  miPublished <- Gen.bool
  miHeroImagePath <- Gen.maybe $ Gen.text (Range.linear 1 100) Gen.alphaNum
  pure (miTitle, miContent, miPublished, miHeroImagePath)

userInsertGen :: (MonadIO m, MonadGen m) => m User.ModelInsert
userInsertGen = do
  miEmail <- genEmail
  miDisplayName <- genDisplayName
  miPassword <- genPassword
  miAvatarUrl <- Gen.maybe $ Gen.text (Range.linear 1 10) Gen.alphaNum
  miIsAdmin <- Gen.bool
  pure User.ModelInsert {..}

blogPostUpdateGen :: (MonadIO m, MonadGen m) => m (Text, Text, Bool, Maybe Text)
blogPostUpdateGen = do
  muTitle <- Gen.text (Range.linear 1 10) Gen.alphaNum
  muContent <- Gen.text (Range.linear 1 10) Gen.alphaNum
  muPublished <- Gen.bool
  muHeroImagePath <- Gen.maybe $ Gen.text (Range.linear 1 100) Gen.alphaNum
  pure (muTitle, muContent, muPublished, muHeroImagePath)
