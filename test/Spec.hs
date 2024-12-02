module Main where

--------------------------------------------------------------------------------

import API.User.DeleteSpec qualified as User.Delete
import API.User.GetSpec qualified as User.Get
import API.User.Id.GetSpec qualified as User.Id.Get
import Data.Maybe (fromMaybe)
import Effects.Database.Tables.MailingListSpec qualified as User
import Effects.Database.Tables.ServerSessionsSpec qualified as ServerSessions
import Effects.Database.Tables.UserSpec qualified as MailingList
import System.Environment (lookupEnv)
import Test.Database.Setup (withTmpPG)
import Test.Hspec
import Test.Hspec.Api.Formatters.V3 (specdoc, useFormatter)
import Test.Hspec.Runner (Config (..), hspecWith)
import Test.Hspec.Runner qualified as TR
import Test.Text.XmlHtml.Optics (spec)
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

  hspec spec

  withTmpPG $ hspecWith cfg $ parallel $ do
    MailingList.spec
    ServerSessions.spec
    User.spec
    User.Delete.spec
    User.Get.spec
    User.Id.Get.spec
