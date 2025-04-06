module Effects.Database.Tables.BlogPostsSpec where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.BlogPosts qualified as UUT
import Effects.Database.Tables.Images qualified as Images
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
    describe "Effects.Database.Tables.BlogPosts" $ do
      runs 30 . it "insert ; select" $ hedgehog . prop_insertSelect
      runs 30 . it "insert ; update ; select" $ hedgehog . prop_insertUpdateSelect
      runs 30 . it "insert ; delete ; select" $ hedgehog . prop_insertDeleteSelect

prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    userInsert <- forAllT userInsertGen
    (iTitle, iContent, iPublished, iHeroImageId) <- forAllT blogPostInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow userId) <- TRX.statement () (User.insertUser userInsert)
        (OneRow insertedId) <- TRX.statement () (UUT.insertBlogPost UUT.Insert {UUT.iAuthorId = userId, ..})
        selected <- TRX.statement () (UUT.getBlogPost insertedId)

        pure (userId, insertedId, selected)

      assert $ do
        (userId, insertedId, mSelected) <- assertRight result
        (blogPost, _heroImage) <- assertJust mSelected

        iTitle === UUT.mTitle blogPost
        iContent === UUT.mContent blogPost
        iPublished === UUT.mPublished blogPost
        iHeroImageId === UUT.mHeroImageId blogPost
        userId === UUT.mAuthorId blogPost
        insertedId === UUT.mId blogPost

prop_insertUpdateSelect :: TestDBConfig -> PropertyT IO ()
prop_insertUpdateSelect cfg = do
  arrange (bracketConn cfg) $ do
    userInsert <- forAllT userInsertGen
    (iTitle, iContent, iPublished, iHeroImageId) <- forAllT blogPostInsertGen
    (mTitle, mContent, mPublished, mHeroImageId) <- forAllT blogPostUpdateGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow userId) <- TRX.statement () (User.insertUser userInsert)
        (OneRow insertedId) <- TRX.statement () (UUT.insertBlogPost UUT.Insert {UUT.iAuthorId = userId, ..})
        () <- TRX.statement () (UUT.updateBlogPost UUT.Model {UUT.mId = insertedId, UUT.mAuthorId = userId, ..})
        selected <- TRX.statement () (UUT.getBlogPost insertedId)
        pure (userId, insertedId, selected)

      assert $ do
        (userId, insertedId, mSelected) <- assertRight result
        (blogPost, _heroImage) <- assertJust mSelected

        mTitle === UUT.mTitle blogPost
        mContent === UUT.mContent blogPost
        mPublished === UUT.mPublished blogPost
        mHeroImageId === UUT.mHeroImageId blogPost
        userId === UUT.mAuthorId blogPost
        insertedId === UUT.mId blogPost

prop_insertDeleteSelect :: TestDBConfig -> PropertyT IO ()
prop_insertDeleteSelect cfg = do
  arrange (bracketConn cfg) $ do
    userInsert <- forAllT userInsertGen
    (iTitle, iContent, iPublished, iHeroImageId) <- forAllT blogPostInsertGen
    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow userId) <- TRX.statement () (User.insertUser userInsert)
        (OneRow insertedId) <- TRX.statement () (UUT.insertBlogPost UUT.Insert {UUT.iAuthorId = userId, ..})
        inserted <- TRX.statement () (UUT.getBlogPost insertedId)
        () <- TRX.statement () (UUT.deleteBlogPost insertedId)
        selected <- TRX.statement () (UUT.getBlogPost insertedId)

        pure (userId, inserted, selected)

      assert $ do
        (userId, inserted, selectedAfterDelete) <- assertRight result
        (blogPost, _heroImage) <- assertJust inserted

        userId === UUT.mAuthorId blogPost
        selectedAfterDelete === Nothing

--------------------------------------------------------------------------------

blogPostInsertGen :: (MonadIO m, MonadGen m) => m (UUT.Subject, UUT.Body, Bool, Maybe Images.Id)
blogPostInsertGen = do
  iTitle <- UUT.Subject <$> Gen.text (Range.linear 1 10) Gen.alphaNum
  iContent <- UUT.Body <$> Gen.text (Range.linear 1 10) Gen.alphaNum
  iPublished <- Gen.bool
  pure (iTitle, iContent, iPublished, Nothing)

userInsertGen :: (MonadIO m, MonadGen m) => m User.ModelInsert
userInsertGen = do
  miEmail <- genEmail
  miDisplayName <- genDisplayName
  miFullName <- genFullName
  miPassword <- genPassword
  miAvatarUrl <- Gen.maybe $ Gen.text (Range.linear 1 10) Gen.alphaNum
  miIsAdmin <- Gen.bool
  pure User.ModelInsert {..}

blogPostUpdateGen :: (MonadIO m, MonadGen m) => m (UUT.Subject, UUT.Body, Bool, Maybe Images.Id)
blogPostUpdateGen = do
  muTitle <- UUT.Subject <$> Gen.text (Range.linear 1 10) Gen.alphaNum
  muContent <- UUT.Body <$> Gen.text (Range.linear 1 10) Gen.alphaNum
  muPublished <- Gen.bool
  pure (muTitle, muContent, muPublished, Nothing)
