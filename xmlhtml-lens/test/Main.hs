module Main where

--------------------------------------------------------------------------------

import Test.Hspec
import Test.Text.XmlHtml.Optics qualified as Optics

--------------------------------------------------------------------------------

main :: IO ()
main = do
  hspec Optics.spec
