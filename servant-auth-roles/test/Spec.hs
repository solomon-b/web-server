module Main (main) where

import Servant.Auth.RolesSpec qualified
import Test.Hspec (hspec)

main :: IO ()
main = hspec Servant.Auth.RolesSpec.spec
