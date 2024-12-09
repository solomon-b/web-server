module Main where

--------------------------------------------------------------------------------

import API.User.DeleteSpec qualified as User.Delete
import API.User.GetSpec qualified as User.Get
import API.User.Id.GetSpec qualified as User.Id.Get
import Data.Maybe (fromMaybe)
import Effects.Database.Tables.BlogPostsSpec qualified as BlogPosts
import Effects.Database.Tables.MailingListSpec qualified as MailingList
import Effects.Database.Tables.ServerSessionsSpec qualified as ServerSessions
import Effects.Database.Tables.UserSpec qualified as User
import System.Environment (lookupEnv)
import Test.Database.Setup (withTmpPG)
import Test.Hspec
import Test.Hspec.Api.Formatters.V3 (specdoc, useFormatter)
import Test.Hspec.Runner (Config (..), hspecWith)
import Test.Hspec.Runner qualified as TR
import Text.Read (readMaybe)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  mText <- lookupEnv "TEST_CONCURRENCY"
  let maxResources :: Int
      maxResources = fromMaybe 4 (mText >>= readMaybe)
  let cfg =
        useFormatter ("specdoc", specdoc) $
          TR.defaultConfig
            { configConcurrentJobs = Just maxResources
            }

  withTmpPG $ hspecWith cfg $ parallel $ do
    -- DB Models
    BlogPosts.spec
    MailingList.spec
    ServerSessions.spec
    User.spec

    -- Handlers
    User.Delete.spec
    User.Get.spec
    User.Id.Get.spec
