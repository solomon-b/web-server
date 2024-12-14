module Main where

--------------------------------------------------------------------------------

import Config.FetchersSpec qualified as Fetchers
import Test.Hspec

--------------------------------------------------------------------------------

main :: IO ()
main = do
  hspec Fetchers.spec
