module App.RedactSpec where

--------------------------------------------------------------------------------

import App.Config (PostgresConfig (..))
import App.Redact (isSecretName, redactBody, redactHeaders, redactQuery, redactRequest, redactServerError, redactSql, redactUrl, redacted, renderCommandError, renderUsageError)
import Data.ByteString (ByteString)
import Data.CaseInsensitive qualified as CI
import Data.Password.Argon2 (PasswordHash (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Time (UTCTime (..), fromGregorian)
import Data.UUID qualified as UUID
import Effects.Database.Tables.ServerSessions qualified as ServerSessions
import Effects.Database.Tables.User qualified as User
import Hasql.Pool qualified as Pool
import Hasql.Session qualified as Session
import Hedgehog (Gen, annotate, assert, forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.Wai qualified as Wai
import Servant qualified
import Test.Gen.EmailAddress (genEmail)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

-- | A value that can not occur in the marker or in SQL keywords.
canaryGen :: Gen Text
canaryGen = ("zq" <>) <$> Gen.text (Range.linear 6 20) Gen.alphaNum

spec :: Spec
spec =
  describe "App.Redact" $ do
    describe "redactSql" $ do
      it "replaces a plain string literal" $
        redactSql "SELECT * FROM t WHERE token = 'abc'"
          `shouldBe` "SELECT * FROM t WHERE token = '<redacted>'"

      it "replaces a Rel8 escape string inside a cast" $
        redactSql "WHERE \"token\" = CAST(E'canary-secret-123' AS \"text\")"
          `shouldBe` "WHERE \"token\" = CAST('<redacted>' AS \"text\")"

      it "replaces a string with a doubled quote" $
        redactSql "VALUES ('it''s', 'x')" `shouldBe` "VALUES ('<redacted>', '<redacted>')"

      it "replaces an escape string with a backslash quote" $
        redactSql "VALUES (E'a\\'b', 1)" `shouldBe` "VALUES ('<redacted>', <redacted>)"

      it "replaces bit, hex, national and unicode strings" $
        redactSql "VALUES (B'101', X'1F', N'abc', U&'d\\0061t')"
          `shouldBe` "VALUES ('<redacted>', '<redacted>', '<redacted>', '<redacted>')"

      it "replaces dollar-quoted strings" $
        redactSql "SELECT $$a'b$$, $tag$x$$y$tag$"
          `shouldBe` "SELECT '<redacted>', '<redacted>'"

      it "replaces numeric literals" $
        redactSql "SELECT 42, 3.5, 1e10, 2.5E-3, .5 LIMIT 10"
          `shouldBe` "SELECT <redacted>, <redacted>, <redacted>, <redacted>, <redacted> LIMIT <redacted>"

      it "keeps placeholders, identifiers, keywords and quoted identifiers" $ do
        let input = "SELECT t1.col_2, \"we'ird\" FROM users2 WHERE a = $1 AND b IS NULL AND c = TRUE OR d = FALSE"
        redactSql input `shouldBe` input

      it "keeps comments and does not lose sync on a quote in a comment" $
        redactSql "SELECT a -- it's\n, 'x' /* don't */ FROM t"
          `shouldBe` "SELECT a -- it's\n, '<redacted>' /* don't */ FROM t"

      it "replaces the rest of an unterminated string" $
        redactSql "WHERE a = 'abc" `shouldBe` "WHERE a = '<redacted>'"

      it "does not change SQL with no literals" $ do
        let input = "SELECT id, email FROM users WHERE id = $1"
        redactSql input `shouldBe` input

      it "removes a value in any literal form" $ hedgehog $ do
        canary <- forAll canaryGen
        (open, close) <-
          forAll $
            Gen.element
              [ ("'", "'"),
                ("E'", "'"),
                ("e'", "'"),
                ("$$", "$$"),
                ("$k$", "$k$"),
                ("CAST(E'", "' AS \"text\")")
              ]
        let output = redactSql ("SELECT * FROM t WHERE x = " <> open <> canary <> close)
        annotate (Text.unpack output)
        assert (not (canary `Text.isInfixOf` output))
        assert (redacted `Text.isInfixOf` output)

    describe "isSecretName" $ do
      it "matches names that hold a secret" $
        mapM_
          (\name -> (name, isSecretName name) `shouldBe` (name, True))
          [ "authorization",
            "Proxy-Authorization",
            "cookie",
            "X-Playout-Secret",
            "Stripe-Signature",
            "X-Api-Key",
            "token",
            "reset_token",
            "password",
            "code",
            "sig"
          ]

      it "does not match ordinary names" $
        mapM_
          (\name -> (name, isSecretName name) `shouldBe` (name, False))
          ["accept", "content-type", "user-agent", "host", "page", "redirect", "id"]

    describe "redactHeaders" $ do
      it "replaces secret header values and keeps the others" $
        redactHeaders
          [ ("Authorization", "Bearer canary"),
            ("Cookie", "session-id-production=canary"),
            ("X-Playout-Secret", "canary"),
            ("Stripe-Signature", "t=1,v1=canary"),
            ("Accept", "text/html")
          ]
          `shouldBe` [ ("Authorization", marker),
                       ("Cookie", marker),
                       ("X-Playout-Secret", marker),
                       ("Stripe-Signature", marker),
                       ("Accept", "text/html")
                     ]

      it "replaces secret query values in a URL header" $
        redactHeaders
          [ ("Referer", "https://kpbj.fm/user/reset?token=canary&page=2"),
            ("HX-Current-URL", "https://kpbj.fm/verify?token=canary")
          ]
          `shouldBe` [ ("Referer", "https://kpbj.fm/user/reset?token=" <> marker <> "&page=2"),
                       ("HX-Current-URL", "https://kpbj.fm/verify?token=" <> marker)
                     ]

      it "never keeps a canary in a secret header" $ hedgehog $ do
        canary <- forAll canaryGen
        name <- forAll $ Gen.element ["authorization", "cookie", "x-playout-secret", "stripe-signature", "x-api-key"]
        let output = redactHeaders [(CI.mk name, Text.Encoding.encodeUtf8 canary)]
        annotate (show output)
        assert (not (any (Text.isInfixOf canary . Text.Encoding.decodeUtf8 . snd) output))

    describe "redactQuery" $ do
      it "replaces secret parameter values and keeps the others" $
        redactQuery [("token", Just "canary"), ("page", Just "2"), ("password", Just "canary"), ("flag", Nothing)]
          `shouldBe` [("token", Just marker), ("page", Just "2"), ("password", Just marker), ("flag", Nothing)]

      it "replaces a secret inside a nested URL value" $
        redactQuery [("redirect", Just "/user/reset?token=canary&x=1")]
          `shouldBe` [("redirect", Just ("/user/reset?token=" <> marker <> "&x=1"))]

    describe "redactUrl" $ do
      it "does not change a URL with no query" $
        redactUrl "https://kpbj.fm/blog/1" `shouldBe` "https://kpbj.fm/blog/1"

      it "replaces a percent-encoded secret in a nested URL" $
        redactUrl "/user/login?redirect=%2Fuser%2Freset%3Ftoken%3Dcanary"
          `shouldBe` "/user/login?redirect=/user/reset?token=" <> marker

      it "matches an encoded parameter name" $
        redactUrl "/x?reset%5Ftoken=canary" `shouldBe` "/x?reset%5Ftoken=" <> marker

    describe "redactBody" $ do
      it "keeps an empty body empty" $
        redactBody "" `shouldBe` ""

      it "replaces a body that is not empty" $
        redactBody "email=a%40b.c&password=canary" `shouldBe` redacted

    describe "renderUsageError" $ do
      it "removes parameter values, SQL literals, detail and quoted values" $ do
        let err =
              Pool.SessionUsageError $
                Session.QueryError
                  "SELECT * FROM t WHERE token = CAST(E'canary-sql' AS \"text\") AND id = $1"
                  ["canary-param"]
                  ( Session.ResultError $
                      Session.ServerError
                        "22P02"
                        "invalid input syntax for type uuid: \"canary-msg\""
                        (Just "Key (token)=(canary-detail) already exists.")
                        (Just "canary-hint")
                        (Just 31)
                  )
            output = renderUsageError err
        output
          `shouldBe` "QueryError {sql = SELECT * FROM t WHERE token = CAST('<redacted>' AS \"text\") AND id = $1, params = [<redacted>], error = ServerError {code = 22P02, message = invalid input syntax for type uuid: \"<redacted>\", detail = <redacted>, hint = <redacted>, position = 31}}"
        mapM_ (\c -> (c, c `Text.isInfixOf` output) `shouldBe` (c, False)) ["canary-sql", "canary-param", "canary-msg", "canary-detail", "canary-hint"]

      it "removes quoted values from a connection error" $
        renderUsageError (Pool.ConnectionUsageError (Just "missing \"=\" after \"postgres://u:canary@h/db\" in connection info string"))
          `shouldBe` "ConnectionUsageError: missing \"<redacted>\" after \"<redacted>\" in connection info string"

      it "keeps errors that hold no value" $ do
        renderUsageError Pool.AcquisitionTimeoutUsageError `shouldBe` "AcquisitionTimeoutUsageError"
        renderUsageError (Pool.SessionUsageError (Session.QueryError "SELECT 1" [] (Session.ResultError (Session.UnexpectedAmountOfRows 2))))
          `shouldBe` "QueryError {sql = SELECT <redacted>, params = [], error = UnexpectedAmountOfRows 2}"

      it "removes the text of a row value error" $
        renderUsageError (Pool.SessionUsageError (Session.PipelineError (Session.ResultError (Session.RowError 0 1 (Session.ValueError "bad value canary")))))
          `shouldBe` "PipelineError {error = RowError {row = 0, column = 1, error = ValueError <redacted>}}"

    describe "renderCommandError" $
      it "removes the detail of a server error" $
        renderCommandError (Session.ResultError (Session.ServerError "23505" "duplicate key" (Just "Key (token)=(canary)") Nothing Nothing))
          `shouldBe` "ServerError {code = 23505, message = duplicate key, detail = <redacted>, hint = , position = }"

    describe "redactRequest" $
      it "removes secrets from the Show text of a request" $ do
        let req =
              Wai.defaultRequest
                { Wai.rawPathInfo = "/user/reset",
                  Wai.rawQueryString = "?token=canary-q&page=2",
                  Wai.queryString = [("token", Just "canary-q"), ("page", Just "2")],
                  Wai.requestHeaders = [("Cookie", "session-id-production=canary-c"), ("Referer", "https://x/y?token=canary-r"), ("Accept", "text/html")],
                  Wai.requestHeaderReferer = Just "https://x/y?token=canary-r"
                }
            output = Text.pack (show (redactRequest req))
        mapM_ (\c -> (c, c `Text.isInfixOf` output) `shouldBe` (c, False)) ["canary-q", "canary-c", "canary-r"]
        mapM_ (\c -> (c, c `Text.isInfixOf` output) `shouldBe` (c, True)) ["/user/reset", "page", "text/html"]

    describe "redactServerError" $
      it "removes a session cookie from the error headers" $ do
        let err = Servant.err302 {Servant.errHeaders = [("Set-Cookie", "session-id-production=canary"), ("Location", "/")]}
        Servant.errHeaders (redactServerError err) `shouldBe` [("Set-Cookie", marker), ("Location", "/")]

    describe "Show and Display instances" $ do
      it "PostgresConfig does not show the connection string" $ do
        let output = show (PostgresConfig "postgres://u:canary-pw@h:5432/db")
        output `shouldBe` "PostgresConfig {postgresConfigConnectionString = <redacted>}"

      it "User.Model and User.ModelInsert do not show the password hash" $ hedgehog $ do
        email <- forAll genEmail
        canary <- forAll canaryGen
        let hash = PasswordHash canary
            outputs =
              [ Text.pack (show (User.Model 1 email hash)),
                display (User.Model 1 email hash),
                Text.pack (show (User.ModelInsert email hash)),
                display (User.ModelInsert email hash)
              ]
        annotate (show outputs)
        assert (not (any (canary `Text.isInfixOf`) outputs))

      it "ServerSessions.Id, Model and Domain show only the first 8 characters" $ do
        let uuid = UUID.fromWords 0x1a2b3c4d 0x5e6f7081 0x92a3b4c5 0xd6e7f809
            sId = ServerSessions.Id uuid
            time = UTCTime (fromGregorian 2026 1 1) 0
            model = ServerSessions.Model sId 1 Nothing Nothing time time
            domain = ServerSessions.toDomain model
            rest = Text.drop 8 (UUID.toText uuid)
            outputs = [Text.pack (show sId), Text.pack (show model), display model, Text.pack (show domain), display domain]
        show sId `shouldBe` "Id 1a2b3c4d<redacted>"
        mapM_ (\o -> (o, rest `Text.isInfixOf` o, "1a2b3c4d" `Text.isInfixOf` o) `shouldBe` (o, False, True)) outputs

      it "ServerSessions.Id Display still gives the full id for the cookie" $ do
        let uuid = UUID.fromWords 0x1a2b3c4d 0x5e6f7081 0x92a3b4c5 0xd6e7f809
        display (ServerSessions.Id uuid) `shouldBe` UUID.toText uuid

marker :: ByteString
marker = Text.Encoding.encodeUtf8 redacted
