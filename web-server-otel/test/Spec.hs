module Main where

import Effects.Database.Execute.OtelSpec qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
  Effects.Database.Execute.OtelSpec.spec
