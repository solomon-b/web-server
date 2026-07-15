module Main (main) where

import App.Auth.RoleSpec qualified
import Test.Hspec (hspec)

main :: IO ()
main = hspec App.Auth.RoleSpec.spec
