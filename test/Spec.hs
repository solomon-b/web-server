module Main where

--------------------------------------------------------------------------------

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
    MailingList.spec
    ServerSessions.spec
    User.spec
