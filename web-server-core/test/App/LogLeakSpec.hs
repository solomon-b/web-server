{-# LANGUAGE ScopedTypeVariables #-}

-- | Regression tests for secrets in log output. Each test sends a known
-- canary value through a real log path and examines all of the log output.
-- The test passes only if the canary is not in the output.
module App.LogLeakSpec where

--------------------------------------------------------------------------------

import App (mkWarpSettings)
import App.Config (WarpConfig (..))
import Control.Exception (SomeException)
import Control.Monad.Catch (try)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Functor.Contravariant ((>$<))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuery, execQueryThrow, execQueryThrowMessage)
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as Session
import Hasql.Statement qualified as HSQL
import Log qualified
import Log.Internal.Logger qualified as Log.Internal
import Network.HTTP.Types.Status qualified as Status
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp.Internal qualified as Warp
import Test.Database.Monad (TestDB, TestDBConfig, bracketConnWithLogger, withTestDB)
import Test.Hspec

--------------------------------------------------------------------------------

canary :: Text
canary = "canary-secret-123"

-- | A logger that keeps each message, and a function that returns all of
-- the messages as JSON text.
captureLogger :: IO (Log.LoggerEnv, IO Text)
captureLogger = do
  ref <- newIORef []
  let logger = Log.Internal.Logger (\msg -> modifyIORef' ref (msg :)) (pure ()) (pure ())
      env = Log.LoggerEnv logger "test" [] [] Log.LogTrace
      render = Text.unlines . fmap (TE.decodeUtf8 . BL.toStrict . Aeson.encode) . reverse <$> readIORef ref
  pure (env, render)

-- | Run a database action and return all of its log output. The function
-- ignores an exception from the action.
logsOf :: TestDBConfig -> TestDB a -> IO Text
logsOf cfg action = do
  (env, render) <- captureLogger
  _ <- bracketConnWithLogger env cfg (try @_ @SomeException action)
  render

shouldNotLeak :: Text -> Expectation
shouldNotLeak output = do
  output `shouldSatisfy` (not . Text.null)
  output `shouldSatisfy` (not . Text.isInfixOf canary)

-- | A Rel8 style statement: the value is a literal in the SQL text.
literalStatement :: HSQL.Statement () Int64
literalStatement =
  HSQL.Statement
    (TE.encodeUtf8 ("SELECT count(*) FROM (SELECT CAST(E'" <> canary <> "' AS \"text\") AS t) s"))
    E.noParams
    (D.singleRow (D.column (D.nonNullable D.int8)))
    False

-- | The value is a parameter. Postgres rejects it and puts it in the error.
castFailStatement :: HSQL.Statement () Text
castFailStatement =
  HSQL.Statement
    "SELECT CAST($1 AS uuid)::text"
    (const canary >$< E.param (E.nonNullable E.text))
    (D.singleRow (D.column (D.nonNullable D.text)))
    True

-- | The second insert fails. The Postgres detail holds the value.
insertStatement :: HSQL.Statement () ()
insertStatement =
  HSQL.Statement
    "INSERT INTO leak_test (token) VALUES ($1)"
    (const canary >$< E.param (E.nonNullable E.text))
    D.noResult
    True

spec :: Spec
spec = do
  withTestDB $
    describe "Effects.Database.Execute logs" $ do
      it "do not hold a literal value from the SQL text" $ \cfg ->
        logsOf cfg (execQuery literalStatement) >>= shouldNotLeak

      it "do not hold a parameter value from a failed query" $ \cfg ->
        logsOf cfg (execQueryThrow castFailStatement) >>= shouldNotLeak

      it "do not hold the Postgres detail of a failed query" $ \cfg ->
        shouldNotLeak
          =<< logsOf
            cfg
            ( do
                _ <- runDB (Session.sql "CREATE TEMP TABLE leak_test (token text UNIQUE)")
                _ <- execQuery insertStatement
                execQueryThrowMessage "duplicate" insertStatement
            )

  describe "App.mkWarpSettings request log" $
    it "does not hold a secret header, query parameter, or body" $ do
      (env, render) <- captureLogger
      let settings = mkWarpSettings env (WarpConfig 0 30 30 "test")
          secret = TE.encodeUtf8 canary
      req <-
        Wai.setRequestBodyChunks
          <$> chunksOf ["password=" <> secret]
          <*> pure
            Wai.defaultRequest
              { Wai.rawPathInfo = "/user/reset",
                Wai.queryString = [("token", Just secret), ("redirect", Just ("/reset?token=" <> secret)), ("page", Just "2")],
                Wai.requestHeaders =
                  [ ("Authorization", "Bearer " <> secret),
                    ("Cookie", "session-id-production=" <> secret),
                    ("X-Playout-Secret", secret),
                    ("Stripe-Signature", "t=1,v1=" <> secret),
                    ("Referer", "https://kpbj.fm/user/reset?token=" <> secret),
                    ("Accept", "text/html")
                  ]
              }
      Warp.settingsLogger settings req Status.status200 Nothing
      output <- render
      shouldNotLeak output
      output `shouldSatisfy` Text.isInfixOf "/user/reset"
      output `shouldSatisfy` Text.isInfixOf "text/html"
  where
    -- Return each chunk once, then an empty chunk, as Warp does.
    chunksOf chunks = do
      ref <- newIORef chunks
      pure $ do
        rest <- readIORef ref
        case rest of
          [] -> pure ""
          (c : cs) -> modifyIORef' ref (const cs) >> pure c
