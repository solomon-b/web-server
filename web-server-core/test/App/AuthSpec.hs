module App.AuthSpec where

--------------------------------------------------------------------------------

import App.Auth (mkCookieSession, mkCookieSessionExpired, truncateSessionId)
import App.Config (Environment (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.UUID.V4 (nextRandom)
import Effects.Database.Tables.ServerSessions qualified as ServerSessions
import Hedgehog (Gen, MonadTest, annotate, evalIO, failure, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

envGen :: Gen Environment
envGen = Gen.element [Development, Staging, Production]

spec :: Spec
spec =
  describe "App.Auth" $ do
    describe "mkCookieSession" $ do
      it "contains required cookie attributes" $ hedgehog $ do
        env <- forAll envGen
        sId <- ServerSessions.Id <$> evalIO nextRandom
        let cookie = mkCookieSession env Nothing sId
        assertContains cookie "HttpOnly"
        assertContains cookie "Secure"
        assertContains cookie "SameSite=lax"
        assertContains cookie "Path=/"
        assertContains cookie (display sId)

      it "omits Domain when Nothing" $ hedgehog $ do
        env <- forAll envGen
        sId <- ServerSessions.Id <$> evalIO nextRandom
        let cookie = mkCookieSession env Nothing sId
        assertNotContains cookie "Domain="

      it "includes Domain when Just" $ hedgehog $ do
        env <- forAll envGen
        sId <- ServerSessions.Id <$> evalIO nextRandom
        let cookie = mkCookieSession env (Just "example.com") sId
        assertContains cookie "Domain=example.com"

    describe "mkCookieSessionExpired" $ do
      it "contains Max-Age=0" $ hedgehog $ do
        env <- forAll envGen
        let cookie = mkCookieSessionExpired env Nothing
        assertContains cookie "Max-Age=0"
        assertContains cookie "HttpOnly"
        assertContains cookie "Secure"

      it "clears the cookie value" $ hedgehog $ do
        env <- forAll envGen
        let cookie = mkCookieSessionExpired env Nothing
        assertContains cookie "=; "

    describe "truncateSessionId" $ do
      it "produces 8 chars" $ hedgehog $ do
        sId <- ServerSessions.Id <$> evalIO nextRandom
        Text.length (truncateSessionId sId) === 8

--------------------------------------------------------------------------------

assertContains :: (MonadTest m) => Text -> Text -> m ()
assertContains haystack needle =
  if needle `Text.isInfixOf` haystack
    then pure ()
    else do
      annotate $ "Expected " <> show haystack <> " to contain " <> show needle
      failure

assertNotContains :: (MonadTest m) => Text -> Text -> m ()
assertNotContains haystack needle =
  if needle `Text.isInfixOf` haystack
    then do
      annotate $ "Expected " <> show haystack <> " NOT to contain " <> show needle
      failure
    else pure ()
