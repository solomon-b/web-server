{-# LANGUAGE TypeApplications #-}

module Config.FetchersSpec where

--------------------------------------------------------------------------------

import Config.Fetchers qualified as UUT
import Control.Exception (bracket_)
import Data.Functor.Compose (Compose (..))
import Data.Text (Text)
import System.Environment (setEnv)
import Test.Hspec qualified as Hspec

--------------------------------------------------------------------------------

setEnvVar :: String -> IO a -> IO a
setEnvVar val action = do
  bracket_
    (setEnv "VAR" val)
    (setEnv "VAR" "")
    action

spec :: Hspec.Spec
spec = do
  Hspec.describe "parseEnv" $ do
    Hspec.around_ (setEnvVar "VAL") $
      Hspec.it "Succeeds with an Env Var present" $ do
        result <- getCompose $ UUT.parseEnv Right "VAR"
        result `Hspec.shouldBe` Right "VAL"

    Hspec.it "Fails with no Env Var" $ do
      result <- getCompose $ UUT.parseEnv Right "VAR"
      result `Hspec.shouldBe` Left UUT.MissingEnvVar

    Hspec.around_ (setEnvVar "VAL") $
      Hspec.it "Fails with bad parse" $ do
        setEnv "VAR" "VAL"
        let parser x = if x == "VALx" then Right x else Left UUT.ParseFailure
        result <- getCompose $ UUT.parseEnv parser "VAR"
        result `Hspec.shouldBe` Left UUT.ParseFailure

  Hspec.describe "parseEnvStr" $ do
    Hspec.around_ (setEnvVar "VAL") $
      Hspec.it "Succeeds with an Env Var present" $ do
        result <- getCompose $ UUT.parseEnvStr @Text "VAR"
        result `Hspec.shouldBe` Right "VAL"

    Hspec.it "Fails with no Env Var" $ do
      result <- getCompose $ UUT.parseEnvStr @Text "VAR"
      result `Hspec.shouldBe` Left UUT.MissingEnvVar

  Hspec.describe "parseEnvOptional" $ do
    Hspec.around_ (setEnvVar "VAL") $
      Hspec.it "Succeeds with an Env Var present" $ do
        result <- getCompose $ UUT.parseEnvOptional Right "VAR"
        result `Hspec.shouldBe` Right (Just "VAL")

    Hspec.it "Succeeds with no Env Var" $ do
      result <- getCompose $ UUT.parseEnvOptional Right "VAR"
      result `Hspec.shouldBe` Right Nothing

    Hspec.around_ (setEnvVar "VAL") $
      Hspec.it "Fails with bad parse" $ do
        setEnv "VAR" "VAL"
        let parser x = if x == "VALx" then Right x else Left UUT.ParseFailure
        result <- getCompose $ UUT.parseEnvOptional parser "VAR"
        result `Hspec.shouldBe` Left UUT.ParseFailure

  Hspec.describe "parseEnvOptionalStr" $ do
    Hspec.around_ (setEnvVar "VAL") $
      Hspec.it "Succeeds with an Env Var present" $ do
        result <- getCompose $ UUT.parseEnvOptionalStr @Text "VAR"
        result `Hspec.shouldBe` Right (Just "VAL")

    Hspec.it "Succeeds with no Env Var" $ do
      result <- getCompose $ UUT.parseEnvOptionalStr @Text "VAR"
      result `Hspec.shouldBe` Right Nothing

  Hspec.describe "parseEnvDefault" $ do
    Hspec.around_ (setEnvVar "VAL") $
      Hspec.it "Succeeds with an Env Var present" $ do
        result <- getCompose $ UUT.parseEnvDefault "DEF" Right "VAR"
        result `Hspec.shouldBe` Right "VAL"

    Hspec.it "Succeeds with no Env Var" $ do
      result <- getCompose $ UUT.parseEnvDefault "DEF" Right "VAR"
      result `Hspec.shouldBe` Right "DEF"

    Hspec.around_ (setEnvVar "VAL") $
      Hspec.it "Fails with bad parse" $ do
        setEnv "VAR" "VAL"
        let parser x = if x == "VALx" then Right x else Left UUT.ParseFailure
        result <- getCompose $ UUT.parseEnvDefault "DEF" parser "VAR"
        result `Hspec.shouldBe` Left UUT.ParseFailure

  Hspec.describe "parseEnvDefaultStr" $ do
    Hspec.around_ (setEnvVar "VAL") $
      Hspec.it "Succeeds with an Env Var present" $ do
        result <- getCompose $ UUT.parseEnvDefaultStr @Text "DEF" "VAR"
        result `Hspec.shouldBe` Right "VAL"

    Hspec.it "Succeeds with no Env Var" $ do
      result <- getCompose $ UUT.parseEnvDefaultStr @Text "DEF" "VAR"
      result `Hspec.shouldBe` Right "DEF"

  Hspec.describe "readEnv" $ do
    Hspec.around_ (setEnvVar "123") $
      Hspec.it "Succeeds with an Env Var present" $ do
        result <- getCompose $ UUT.readEnv @Int "VAR"
        result `Hspec.shouldBe` Right 123

    Hspec.it "Fails with no Env Var" $ do
      result <- getCompose $ UUT.readEnv @Int "VAR"
      result `Hspec.shouldBe` Left UUT.MissingEnvVar

    Hspec.around_ (setEnvVar "123") $
      Hspec.it "Fails with bad parse" $ do
        setEnv "VAR" "VAL"
        result <- getCompose $ UUT.readEnv @Int "VAR"
        result `Hspec.shouldBe` Left UUT.ParseFailure

  Hspec.describe "readEnvOptional" $ do
    Hspec.around_ (setEnvVar "123") $
      Hspec.it "Succeeds with an Env Var present" $ do
        result <- getCompose $ UUT.readEnvOptional @Int "VAR"
        result `Hspec.shouldBe` Right (Just 123)

    Hspec.it "Succeeds with no Env Var" $ do
      result <- getCompose $ UUT.readEnvOptional @Int "VAR"
      result `Hspec.shouldBe` Right Nothing

    Hspec.around_ (setEnvVar "VAL") $
      Hspec.it "Fails with bad parse" $ do
        setEnv "VAR" "VAL"
        result <- getCompose $ UUT.readEnvOptional @Int "VAR"
        result `Hspec.shouldBe` Right Nothing

  Hspec.describe "readEnvDefault" $ do
    Hspec.around_ (setEnvVar "123") $
      Hspec.it "Succeeds with an Env Var present" $ do
        result <- getCompose $ UUT.readEnvDefault @Int 420 "VAR"
        result `Hspec.shouldBe` Right 123

    Hspec.it "Succeeds with no Env Var" $ do
      result <- getCompose $ UUT.readEnvDefault @Int 420 "VAR"
      result `Hspec.shouldBe` Right 420

    Hspec.around_ (setEnvVar "VAL") $
      Hspec.it "Fails with bad parse" $ do
        setEnv "VAR" "VAL"
        result <- getCompose $ UUT.readEnvDefault @Int 420 "VAR"
        result `Hspec.shouldBe` Left UUT.ParseFailure
