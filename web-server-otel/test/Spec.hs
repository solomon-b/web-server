module Main where

import Test.Hspec

import qualified Effects.Database.Execute.OtelSpec

main :: IO ()
main = hspec $ do
  Effects.Database.Execute.OtelSpec.spec
