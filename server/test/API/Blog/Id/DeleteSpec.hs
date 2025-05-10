module API.Blog.Id.DeleteSpec where

--------------------------------------------------------------------------------

import API.Blog.Id.Delete qualified as UUT
import App.Auth (Authz)
import Data.Foldable (traverse_)
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.User qualified as User
import Hasql.Interpolate (OneRow (..))
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog.Internal.Property (forAllT, (===))
import Test.Database.Monad (TestDBConfig, bracketConn, withAuth, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Gen.Tables.BlogPosts (blogPostInsertGen)
import Test.Gen.Tables.Users (userInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (PropertyT, hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    withAuth $
      describe "API.Blog.Id.Delete" $ do
        runs 30 . it "deletes the expected blog post" $ \(auth, cfg) -> hedgehog $ prop_insertSelect auth cfg

prop_insertSelect :: Authz -> TestDBConfig -> PropertyT IO ()
prop_insertSelect auth cfg = do
  arrange (bracketConn cfg) $ do
    userInserts <- forAllT userInsertGen
    (iTitle, iContent, iPublished, iHeroImageId) <- forAllT blogPostInsertGen

    act $ do
      -- Insert a user
      OneRow userId <-
        fmap (either (error "user insert failed in test setup") id) $
          runDB $
            TRX.transaction TRX.ReadCommitted TRX.Write $
              TRX.statement () $
                User.insertUser userInserts

      -- Insert a blog post
      blogPostToDelete <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        pid <- TRX.statement () (BlogPosts.insertBlogPost BlogPosts.Insert {BlogPosts.iAuthorId = userId, ..})

        post <- fmap (fromMaybe $ error "oops") $ TRX.statement () $ BlogPosts.getBlogPost $ getOneRow pid
        pure (pid, post)

      -- Fetch all blog posts
      allBlogPosts <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Read $ TRX.statement () BlogPosts.getBlogPosts

      -- Delete the inserted blog post
      traverse_ (UUT.handler auth . getOneRow . fst) blogPostToDelete

      -- Fetch remaining blog posts
      remBlogPosts <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Read $ TRX.statement () BlogPosts.getBlogPosts

      -- The diff should be the deleted blogpost
      let diff = liftA2 (\\) allBlogPosts remBlogPosts
      assert $ diff === fmap (pure . snd) blogPostToDelete
