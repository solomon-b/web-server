{-# LANGUAGE MultiWayIf #-}

-- | Functions that remove secrets from text before it goes to a log.
--
-- Each function replaces a secret value with 'redacted' and keeps the rest
-- of the text. Log lines can go to people who are not server admins. Use
-- these functions on all request data, SQL text, and database errors that
-- you log.
module App.Redact
  ( -- * Marker
    redacted,

    -- * SQL
    redactSql,

    -- * HTTP requests
    isSecretName,
    redactHeaders,
    redactQuery,
    redactUrl,
    redactBody,
    redactRequest,
    redactServerError,

    -- * Database errors
    renderUsageError,
    renderCommandError,
  )
where

--------------------------------------------------------------------------------

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.CaseInsensitive qualified as CI
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as Builder
import Hasql.Pool qualified as Pool
import Hasql.Session qualified as Session
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Servant qualified

--------------------------------------------------------------------------------
-- Marker

-- | The fixed text that takes the place of a secret value.
redacted :: Text
redacted = "<redacted>"

redactedBytes :: ByteString
redactedBytes = Text.Encoding.encodeUtf8 redacted

--------------------------------------------------------------------------------
-- SQL

-- | Replace each literal value in a SQL statement with 'redacted'.
--
-- Rel8 writes values into the SQL text, for example
-- @CAST(E'token' AS \"text\")@. This function replaces these literals:
--
-- * String constants: @'...'@, @E'...'@, @B'...'@, @X'...'@, @N'...'@ and
--   @U&'...'@. Each one becomes @'\<redacted\>'@.
-- * Dollar-quoted strings: @$$...$$@ and @$tag$...$tag$@. Each one becomes
--   @'\<redacted\>'@.
-- * Numeric constants: @42@, @3.5@, @1e10@ and @0x1F@. Each one becomes
--   @\<redacted\>@.
--
-- Identifiers, quoted identifiers, keywords, @NULL@, @TRUE@, @FALSE@,
-- comments and @$1@ placeholders do not change. If a string has no end, the
-- function replaces all of the text after its start.
redactSql :: Text -> Text
redactSql = Lazy.toStrict . Builder.toLazyText . go False
  where
    -- The flag is 'True' when the previous character is part of a name. Then
    -- a digit or a quote prefix is part of that name, not a literal.
    go :: Bool -> Text -> Builder
    go afterName input = case Text.uncons input of
      Nothing -> mempty
      Just (c, rest)
        | c == '\'' ->
            stringMarker <> go False (skipString False rest)
        | not afterName,
          Just (escapes, rest') <- stringPrefix input ->
            stringMarker <> go False (skipString escapes rest')
        | c == '"' ->
            let (name, rest') = quotedName rest
             in Builder.singleton '"' <> Builder.fromText name <> go True rest'
        | c == '$',
          Just rest' <- skipDollarString input ->
            stringMarker <> go False rest'
        | "--" `Text.isPrefixOf` input ->
            let (comment, rest') = Text.break (== '\n') input
             in Builder.fromText comment <> go False rest'
        | "/*" `Text.isPrefixOf` input ->
            let (comment, rest') = Text.breakOn "*/" input
             in Builder.fromText comment <> Builder.fromText (Text.take 2 rest') <> go False (Text.drop 2 rest')
        | not afterName,
          startsNumber c rest ->
            Builder.fromText redacted <> go False (skipNumber input)
        | otherwise ->
            Builder.singleton c <> go (isNameChar c) rest

    stringMarker :: Builder
    stringMarker = "'" <> Builder.fromText redacted <> "'"

    isNameChar :: Char -> Bool
    isNameChar c = isAlphaNum c || c == '_' || c == '$'

    -- A prefix before a quote. The 'Bool' is 'True' for an escape string,
    -- where a backslash escapes the next character.
    stringPrefix :: Text -> Maybe (Bool, Text)
    stringPrefix input =
      case Text.uncons input of
        Just (p, rest)
          | p `elem` ("Ee" :: String), Just rest' <- Text.stripPrefix "'" rest -> Just (True, rest')
          | p `elem` ("BbXxNn" :: String), Just rest' <- Text.stripPrefix "'" rest -> Just (False, rest')
          | p `elem` ("Uu" :: String), Just rest' <- Text.stripPrefix "&'" rest -> Just (False, rest')
        _ -> Nothing

    -- Skip to the text after the closing quote. Two quotes together are one
    -- quote inside the string.
    skipString :: Bool -> Text -> Text
    skipString escapes input = case Text.uncons input of
      Nothing -> ""
      Just ('\\', rest) | escapes -> skipString escapes (Text.drop 1 rest)
      Just ('\'', rest) -> case Text.uncons rest of
        Just ('\'', rest') -> skipString escapes rest'
        _ -> rest
      Just (_, rest) -> skipString escapes rest

    -- Return the name up to and with its closing quote, and the text after it.
    quotedName :: Text -> (Text, Text)
    quotedName input =
      let (name, rest) = Text.breakOn "\"" input
       in case Text.stripPrefix "\"\"" rest of
            Just rest' ->
              let (name', rest'') = quotedName rest'
               in (name <> "\"\"" <> name', rest'')
            Nothing
              | Text.null rest -> (name, "")
              | otherwise -> (name <> "\"", Text.drop 1 rest)

    -- If the input starts a dollar-quoted string, return the text after it.
    -- A tag can not start with a digit, thus @$1@ is not a string.
    skipDollarString :: Text -> Maybe Text
    skipDollarString input = do
      afterDollar <- Text.stripPrefix "$" input
      let tag = Text.takeWhile (\c -> isAlphaNum c || c == '_') afterDollar
      body <- Text.stripPrefix "$" (Text.drop (Text.length tag) afterDollar)
      case Text.uncons tag of
        Just (first, _) | not (isAlpha first || first == '_') -> Nothing
        _ ->
          let delimiter = "$" <> tag <> "$"
              (_, rest) = Text.breakOn delimiter body
           in Just (Text.drop (Text.length delimiter) rest)

    startsNumber :: Char -> Text -> Bool
    startsNumber c rest =
      isDigit c || (c == '.' && maybe False (isDigit . fst) (Text.uncons rest))

    skipNumber :: Text -> Text
    skipNumber input
      | any (`Text.isPrefixOf` input) ["0x", "0X", "0o", "0O", "0b", "0B"] =
          Text.dropWhile (\c -> isAlphaNum c || c == '_') (Text.drop 2 input)
      | otherwise =
          skipExponent (Text.dropWhile (\c -> isDigit c || c == '.' || c == '_') input)

    skipExponent :: Text -> Text
    skipExponent input = case Text.uncons input of
      Just (e, rest)
        | e == 'e' || e == 'E' ->
            let digits = fromMaybe rest (Text.stripPrefix "+" rest <|> Text.stripPrefix "-" rest)
             in if maybe False (isDigit . fst) (Text.uncons digits)
                  then Text.dropWhile isDigit digits
                  else input
      _ -> input

--------------------------------------------------------------------------------
-- HTTP requests

-- | Parts of a header or parameter name that show a secret value. A name
-- that contains one of these parts, in upper or lower case, is secret.
secretNameParts :: [ByteString]
secretNameParts =
  [ "auth",
    "cookie",
    "credential",
    "csrf",
    "key",
    "passwd",
    "password",
    "secret",
    "session",
    "signature",
    "token"
  ]

-- | Short names that hold a secret, but that are also parts of ordinary
-- words. A name must be equal to one of these, in upper or lower case.
secretNames :: [ByteString]
secretNames =
  [ "code",
    "jwt",
    "otp",
    "pass",
    "pwd",
    "sig"
  ]

-- | Request headers that hold a full URL. A browser sends the URL of the
-- current page in these headers. A password-reset page thus sends its token.
urlHeaders :: [HTTP.HeaderName]
urlHeaders =
  [ "hx-current-url",
    "referer",
    "x-forwarded-uri",
    "x-original-url"
  ]

-- | 'True' if a header or a query parameter with this name holds a secret.
--
-- The rule matches @Authorization@, @Cookie@, @X-Playout-Secret@,
-- @Stripe-Signature@, @X-Api-Key@, @token@, @reset_token@, @password@, OAuth
-- @code@, and similar names. The rule can also match an ordinary name, for
-- example @monkey@. Then the log loses that value, but no secret leaks.
isSecretName :: ByteString -> Bool
isSecretName name =
  let folded = CI.foldCase name
   in folded `elem` secretNames || any (`ByteString.isInfixOf` folded) secretNameParts

-- | Replace the value of each secret header with 'redacted'. In a header that
-- holds a URL, replace the value of each secret query parameter.
redactHeaders :: [HTTP.Header] -> [HTTP.Header]
redactHeaders = fmap redactHeader
  where
    redactHeader (name, value)
      | isSecretName (CI.original name) = (name, redactedBytes)
      | name `elem` urlHeaders = (name, redactUrl value)
      | otherwise = (name, value)

-- | Replace the value of each secret query parameter with 'redacted'. If a
-- value is a URL with a query, replace the secrets in that URL too. Use this
-- function on a decoded query, for example 'Network.Wai.queryString'.
redactQuery :: HTTP.Query -> HTTP.Query
redactQuery = fmap redactParam
  where
    redactParam (key, value)
      | isSecretName key = (key, redactedBytes <$ value)
      | otherwise = (key, redactNestedUrl <$> value)

-- | Replace the value of each secret query parameter in a URL with
-- 'redacted'. A URL with no query does not change.
--
-- The function does not decode or encode a parameter that does not change.
-- If a parameter value holds a URL with a secret, the result holds the
-- decoded form of that URL, with the secret replaced.
redactUrl :: ByteString -> ByteString
redactUrl url =
  case Char8.break (== '?') url of
    (_, "") -> url
    (base, query) -> base <> "?" <> redactRawQuery (ByteString.drop 1 query)
  where
    redactRawQuery = ByteString.intercalate "&" . fmap redactPair . Char8.split '&'

    redactPair pair =
      let (rawKey, rawValue) = Char8.break (== '=') pair
          value = HTTP.urlDecode True (ByteString.drop 1 rawValue)
          value' = redactNestedUrl value
       in if
            | ByteString.null rawValue -> pair
            | isSecretName (HTTP.urlDecode True rawKey) -> rawKey <> "=" <> redactedBytes
            | value' /= value -> rawKey <> "=" <> value'
            | otherwise -> pair

-- | If a value is a URL with a query, replace the secrets in it.
redactNestedUrl :: ByteString -> ByteString
redactNestedUrl value
  | Char8.elem '?' value = redactUrl value
  | otherwise = value

-- | Replace a request body that is not empty with 'redacted'. A body can hold
-- a password, a token, or a webhook payload, thus the function does not keep
-- any part of it.
redactBody :: ByteString -> Text
redactBody body
  | ByteString.null body = ""
  | otherwise = redacted

-- | Remove secrets from the headers, the query, and the referer of a
-- request. Use this function before you 'show' a request, for example in a
-- trace span.
redactRequest :: Wai.Request -> Wai.Request
redactRequest req =
  req
    { Wai.requestHeaders = redactHeaders (Wai.requestHeaders req),
      Wai.rawQueryString = redactUrl (Wai.rawQueryString req),
      Wai.queryString = redactQuery (Wai.queryString req),
      Wai.requestHeaderReferer = redactUrl <$> Wai.requestHeaderReferer req
    }

-- | Remove secrets from the headers of a Servant error. A redirect error can
-- carry a @Set-Cookie@ header with a session id. Use this function before
-- you 'show' an error.
redactServerError :: Servant.ServerError -> Servant.ServerError
redactServerError err = err {Servant.errHeaders = redactHeaders (Servant.errHeaders err)}

--------------------------------------------------------------------------------
-- Database errors

-- | Render a database error for a log, with no secret values. Use this
-- function in place of 'show'.
--
-- The 'show' text of a hasql error holds the SQL text and the value of each
-- statement parameter. A parameter can be a session id, a token, or a
-- password hash. This function keeps the kind of error, the SQL text with
-- 'redactSql', the SQLSTATE code, and the position. It replaces these parts
-- with 'redacted':
--
-- * each parameter value
-- * the Postgres @detail@ and @hint@, which can hold row values
-- * each double-quoted part of a message, which can hold an input value
-- * the text of a row value error
renderUsageError :: Pool.UsageError -> Text
renderUsageError = \case
  Pool.ConnectionUsageError Nothing -> "ConnectionUsageError"
  Pool.ConnectionUsageError (Just message) -> "ConnectionUsageError: " <> redactQuoted (decodeLenient message)
  Pool.AcquisitionTimeoutUsageError -> "AcquisitionTimeoutUsageError"
  Pool.SessionUsageError (Session.QueryError statement params err) ->
    "QueryError {sql = "
      <> redactSql (decodeLenient statement)
      <> ", params = ["
      <> Text.intercalate ", " (redacted <$ params)
      <> "], error = "
      <> renderCommandError err
      <> "}"
  Pool.SessionUsageError (Session.PipelineError err) ->
    "PipelineError {error = " <> renderCommandError err <> "}"

-- | Render the command part of a database error for a log, with no secret
-- values. See 'renderUsageError'.
renderCommandError :: Session.CommandError -> Text
renderCommandError = \case
  Session.ClientError Nothing -> "ClientError"
  Session.ClientError (Just message) -> "ClientError: " <> redactQuoted (decodeLenient message)
  Session.ResultError (Session.ServerError code message detail hint position) ->
    "ServerError {code = "
      <> decodeLenient code
      <> ", message = "
      <> redactQuoted (decodeLenient message)
      <> ", detail = "
      <> maybe "" (const redacted) detail
      <> ", hint = "
      <> maybe "" (const redacted) hint
      <> ", position = "
      <> maybe "" (Text.pack . show) position
      <> "}"
  Session.ResultError (Session.UnexpectedResult message) ->
    "UnexpectedResult: " <> redactQuoted message
  Session.ResultError (Session.RowError row column err) ->
    "RowError {row = "
      <> Text.pack (show row)
      <> ", column = "
      <> Text.pack (show column)
      <> ", error = "
      <> renderRowError err
      <> "}"
  Session.ResultError (Session.UnexpectedAmountOfRows n) ->
    "UnexpectedAmountOfRows " <> Text.pack (show n)
  where
    renderRowError = \case
      Session.EndOfInput -> "EndOfInput"
      Session.UnexpectedNull -> "UnexpectedNull"
      Session.ValueError _ -> "ValueError " <> redacted

decodeLenient :: ByteString -> Text
decodeLenient = Text.Encoding.decodeUtf8Lenient

-- | Replace each double-quoted part of a message with 'redacted'. Postgres
-- and libpq put input values in double quotes. If a quote has no end, the
-- function replaces all of the text after it.
redactQuoted :: Text -> Text
redactQuoted message =
  case Text.splitOn "\"" message of
    [] -> message
    (first : rest) -> Text.intercalate "\"" (first : zipWith hide [(1 :: Int) ..] rest)
  where
    hide n part = if odd n then redacted else part
