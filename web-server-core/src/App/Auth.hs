{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App.Auth
  ( -- * Cookie Session Management
    sessionCookieName,
    sessionCookieNameText,
    mkCookieSession,
    mkCookieSessionExpired,
    truncateSessionId,

    -- * Authentication
    authHandler,
    authHandlerWith,
    SessionConfig (..),
    defaultSessionConfig,
    Authz (..),
    AuthErr (..),
    getAuth,
    login,
    expireServerSession,
    lookupSessionId,
    userLoginState,
    LoggedIn (..),
    isLoggedIn,

    -- * Database Utilities
    runDB,
    execStatement,
    healthCheck,
    getSessionUser,

    -- * Utilities
    parseSessionId,
    mkRedirect,
    mkIpRange,
  )
where

--------------------------------------------------------------------------------

import App.Config (Environment (..))
import App.Errors (InternalServerError (..), ToServerError (..), throwErr, toErrorBody)
import Control.Error (note)
import Control.Monad (void, when, (>=>))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader qualified as Reader
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.Foldable (fold)
import Data.Functor ((<&>))
import Data.Has (Has)
import Data.Has qualified as Has
import Data.IP (IP (..), IPRange (..), fromSockAddr, makeAddrRange)
import Data.Password.Argon2 (Argon2, PasswordHash)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime, nominalDay)
import Data.UUID (fromASCIIBytes)
import Domain.Types.EmailAddress (EmailAddress)
import Effects.Database.Tables.ServerSessions qualified as ServerSessions
import Effects.Database.Tables.User qualified as User
import Hasql.Interpolate (getOneRow, interp, sql)
import Hasql.Pool qualified as HSQL
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Session qualified as HSQL
import Hasql.Statement (Statement)
import Log qualified
import Log.Backend.StandardOutput qualified as Log
import Network.Socket (SockAddr)
import Network.Wai (Request (..))
import OrphanInstances.IPRange ()
import OrphanInstances.Password ()
import OrphanInstances.UTCTime ()
import OrphanInstances.UUID ()
import Servant qualified
import Servant.Server.Experimental.Auth (mkAuthHandler)
import Servant.Server.Experimental.Auth qualified as Servant
import Web.Cookie (parseCookies)

--------------------------------------------------------------------------------

parseSessionId :: ByteString -> Maybe ServerSessions.Id
parseSessionId = fmap ServerSessions.Id . fromASCIIBytes

-- | First 8 characters of the session UUID for safe inclusion in logs.
truncateSessionId :: ServerSessions.Id -> Text
truncateSessionId = Text.take 8 . display

--------------------------------------------------------------------------------

-- | Configuration for session idle timeout behavior.
data SessionConfig = SessionConfig
  { -- | Maximum time a session can be idle before it is considered expired.
    -- Default: 30 minutes.
    sessionIdleTimeout :: NominalDiffTime,
    -- | Minimum interval between @last_activity_at@ updates.
    -- Prevents a database write on every single authenticated request.
    -- Default: 5 minutes.
    sessionActivityThrottle :: NominalDiffTime
  }

-- | Sensible defaults: 30 minute idle timeout, 5 minute update throttle.
defaultSessionConfig :: SessionConfig
defaultSessionConfig =
  SessionConfig
    { sessionIdleTimeout = 30 * 60,
      sessionActivityThrottle = 5 * 60
    }

--------------------------------------------------------------------------------

-- | Generate the session cookie name based on the environment.
-- This prevents session conflicts when running multiple environments
-- (dev/staging/prod) on the same domain or localhost.
sessionCookieName :: Environment -> ByteString
sessionCookieName = \case
  Development -> "session-id-development"
  Staging -> "session-id-staging"
  Production -> "session-id-production"

-- | Text version of 'sessionCookieName'.
sessionCookieNameText :: Environment -> Text
sessionCookieNameText = Text.Encoding.decodeUtf8 . sessionCookieName

--------------------------------------------------------------------------------

runDB ::
  ( MonadIO m,
    MonadReader env m,
    Has HSQL.Pool.Pool env
  ) =>
  HSQL.Session a ->
  m (Either HSQL.Pool.UsageError a)
runDB s = do
  pool <- Reader.asks Has.getter
  liftIO $ HSQL.Pool.use pool s

execStatement ::
  ( MonadIO m,
    MonadReader env m,
    Has HSQL.Pool.Pool env
  ) =>
  Statement () a ->
  m (Either HSQL.Pool.UsageError a)
execStatement = runDB . HSQL.statement ()

healthCheck :: Statement () ()
healthCheck = interp False [sql|select current_timestamp|]

getSessionUser :: ServerSessions.Id -> Statement () (Maybe (User.Model, ServerSessions.Model))
getSessionUser sId =
  fmap fromRows
    <$> interp
      False
      [sql|
    SELECT
      u.id as user_id, u.email, u.password,
      s.id as server_session_id, s.user_id as session_user_id, s.ip_address, s.user_agent, s.expires_at, s.last_activity_at
    FROM server_sessions s
    JOIN users u ON u.id = s.user_id
    WHERE s.id = #{sId}
      AND NOW() < expires_at
  |]
  where
    fromRows ::
      ( User.Id,
        EmailAddress,
        Data.Password.Argon2.PasswordHash Data.Password.Argon2.Argon2,
        ServerSessions.Id,
        User.Id,
        Maybe IPRange,
        Maybe Text,
        UTCTime,
        UTCTime
      ) ->
      (User.Model, ServerSessions.Model)
    fromRows
      ( mId,
        mEmail,
        mPassword,
        mSessionId,
        sessionUserId,
        mIpAddress,
        mUserAgent,
        mExpiresAt,
        mLastActivityAt
        ) = (User.Model {..}, ServerSessions.Model {mUserId = sessionUserId, ..})

--------------------------------------------------------------------------------

-- NOTE: If we remove this we could allow downstream consumers of the library to
-- define their own Authz type.
type instance Servant.AuthServerData (Servant.AuthProtect "cookie-auth") = Authz

data Authz = Authz
  { authzUser :: User.Model,
    authzSession :: ServerSessions.Model
  }

data AuthErr
  = MissingCookieHeader
  | MissingCookieValue
  | MalformedSessionId
  deriving (Show)

instance ToServerError AuthErr where
  toServerError = \case
    MissingCookieHeader -> Servant.err307 {Servant.errBody = toErrorBody "No cookie sent in request" 307, Servant.errHeaders = [("Location", "/user/login")]}
    MissingCookieValue -> Servant.err307 {Servant.errBody = toErrorBody "Invalid cookie" 307, Servant.errHeaders = [("Location", "/user/login")]}
    MalformedSessionId -> Servant.err307 {Servant.errBody = toErrorBody "Bad session data" 307, Servant.errHeaders = [("Location", "/user/login")]}
  toServerLog = \case
    MissingCookieHeader -> ("Missing Cookie Header", Just (Aeson.object [("details", "No cookie sent in request")]))
    MissingCookieValue -> ("Missing Cookie Value", Just (Aeson.object [("details", "Invalid cookie")]))
    MalformedSessionId -> ("Malformed Session Id", Just (Aeson.object [("details", "Bad session data")]))

authHandler :: HSQL.Pool -> Environment -> Servant.AuthHandler Request Authz
authHandler pool env = authHandlerWith pool env defaultSessionConfig

authHandlerWith :: HSQL.Pool -> Environment -> SessionConfig -> Servant.AuthHandler Request Authz
authHandlerWith pool env cfg = mkAuthHandler $ \req ->
  let cookieName = sessionCookieName env
      eSession = do
        cookie <- note MissingCookieHeader $ lookup "cookie" $ requestHeaders req
        sessionId <- note MissingCookieValue $ lookup cookieName $ parseCookies cookie
        note MalformedSessionId $ parseSessionId sessionId
      redirect = mkRedirect req
   in do
        case eSession of
          -- Valid auth session_id exists in user's cookie:
          Right sessionId ->
            liftIO (HSQL.use pool (HSQL.statement () $ getSessionUser sessionId)) >>= \case
              -- Internal Server Error: Database usage error:
              Left err -> do
                -- TODO: Propagate logger to here:
                liftIO $ Log.withJsonStdOutLogger $ \stdOutLogger ->
                  Log.runLogT "webserver-backend" stdOutLogger Log.LogAttention $ throwErr $ InternalServerError $ Text.pack $ show err
              -- Auth Failure: Auth session or user do not exist in the DB:
              -- Note: this is a 307 because we redirect on auth failure.
              Right Nothing ->
                Servant.throwError Servant.err401 --  $ Servant.err307 {Servant.errHeaders = [("Location", redirect)]}
                -- Auth Success: check idle timeout, then throttle activity update.
              Right (Just (userModel, sessionModel)) -> do
                now <- liftIO getCurrentTime
                let timeSinceActivity = diffUTCTime now (ServerSessions.mLastActivityAt sessionModel)
                -- Idle timeout exceeded:
                if timeSinceActivity > sessionIdleTimeout cfg
                  then do
                    liftIO $ Log.withJsonStdOutLogger $ \stdOutLogger ->
                      Log.runLogT "webserver-backend" stdOutLogger Log.LogInfo $
                        Log.logInfo "Session Idle Timeout" $
                          Aeson.toJSON $
                            KeyMap.fromList
                              [ "session" .= truncateSessionId sessionId,
                                "user_id" .= display (User.mId userModel),
                                "idle_seconds" .= (round timeSinceActivity :: Int)
                              ]
                    Servant.throwError Servant.err401
                  else do
                    -- Throttled activity update:
                    when (timeSinceActivity > sessionActivityThrottle cfg) $
                      void $
                        liftIO $
                          HSQL.use pool (HSQL.statement () $ ServerSessions.touchSession sessionId)
                    pure $ Authz userModel sessionModel
          -- Auth Failure: Invalid or missing auth session_id in user's cookie:
          Left MissingCookieHeader -> Servant.throwError Servant.err307 {Servant.errBody = "No cookie sent in request", Servant.errHeaders = [("Location", redirect)]}
          Left MissingCookieValue -> Servant.throwError Servant.err307 {Servant.errBody = "Invalid cookie", Servant.errHeaders = [("Location", redirect)]}
          Left MalformedSessionId -> Servant.throwError Servant.err307 {Servant.errBody = "Bad session data", Servant.errHeaders = [("Location", redirect)]}

mkRedirect :: Request -> ByteString
mkRedirect req =
  let reqPath = rawPathInfo req
      reqQuery = rawQueryString req
      requestPath = reqPath <> reqQuery
   in "/user/login?redirect=" <> requestPath

getAuth ::
  ( MonadIO m,
    MonadReader r m,
    Has HSQL.Pool.Pool r
  ) =>
  ServerSessions.Id ->
  m (Either HSQL.UsageError (Maybe Authz))
getAuth sessionId = do
  eSessionData <- runDB . HSQL.statement () $ getSessionUser sessionId
  pure $
    eSessionData
      <&> fmap
        ( \(user, serverSession) ->
            Authz
              { authzUser = user,
                authzSession = serverSession
              }
        )

login ::
  ( MonadIO m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Log.MonadLog m
  ) =>
  User.Id ->
  SockAddr ->
  Maybe Text ->
  m (Either HSQL.UsageError ServerSessions.Id)
login uid sockAddr mUserAgent = do
  now <- liftIO getCurrentTime
  let sessionExpiration = addUTCTime nominalDay now
      sessionToInsert = ServerSessions.ServerSessionInsert uid (mkIpRange sockAddr) mUserAgent sessionExpiration
  result <- fmap (fmap (ServerSessions.mSessionId . getOneRow)) $ execStatement $ ServerSessions.insertServerSession sessionToInsert
  case result of
    Right sId ->
      Log.logInfo "Session Created" $
        Aeson.toJSON $
          KeyMap.fromList
            [ "session" .= truncateSessionId sId,
              "user_id" .= display uid,
              "ip" .= show sockAddr,
              "user_agent" .= mUserAgent
            ]
    Left _ -> pure ()
  pure result

mkIpRange :: SockAddr -> Maybe IPRange
mkIpRange sockAddr =
  fromSockAddr sockAddr <&> \case
    (IPv4 ip, _) -> IPv4Range $ makeAddrRange ip 0
    (IPv6 ip, _) -> IPv6Range $ makeAddrRange ip 0

expireServerSession ::
  ( MonadIO m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Log.MonadLog m
  ) =>
  ServerSessions.Id ->
  m (Either HSQL.UsageError ())
expireServerSession sId = do
  result <- execStatement $ ServerSessions.expireSession sId
  case result of
    Right () ->
      Log.logInfo "Session Logout" $
        Aeson.toJSON $
          KeyMap.fromList ["session" .= truncateSessionId sId]
    Left _ -> pure ()
  pure result

lookupSessionId :: Environment -> Text -> Maybe ServerSessions.Id
lookupSessionId env =
  lookup (sessionCookieName env) . parseCookies . Text.Encoding.encodeUtf8 >=> parseSessionId

data LoggedIn = IsLoggedIn User.Model | IsNotLoggedIn

isLoggedIn :: LoggedIn -> Bool
isLoggedIn = \case
  IsLoggedIn _ -> True
  IsNotLoggedIn -> False

userLoginState ::
  ( MonadIO m,
    MonadReader r m,
    Has HSQL.Pool.Pool r
  ) =>
  Environment ->
  Maybe Text ->
  m LoggedIn
userLoginState env cookie = do
  let mSessionId = cookie >>= lookupSessionId env
  traverse getAuth mSessionId >>= \case
    Just (Right (Just Authz {..})) ->
      pure $ IsLoggedIn authzUser
    _ ->
      pure IsNotLoggedIn

mkCookieSession :: Environment -> Maybe Text -> ServerSessions.Id -> Text
mkCookieSession env mDomain sId =
  fold $
    [ sessionCookieNameText env,
      "=",
      display sId,
      "; ",
      -- NOTE: We have to do this because of OIDC authentication flows
      -- We could remove this if there was web native referrer domain whitelisting.
      -- Instead we implement that in our auth handler. For more info see:
      -- - https://stackoverflow.com/questions/64985696/samesite-cookie-but-allow-specific-domain
      -- - https://github.com/WICG/first-party-sets/
      "SameSite=lax",
      "; ",
      "Path=/",
      "; ",
      "HttpOnly",
      "; ",
      "Secure"
    ]
      <> maybe [] (\domain -> ["; ", "Domain=", domain]) mDomain

-- | Build a @Set-Cookie@ header value that clears the session cookie.
-- Use this in logout responses to remove the cookie from the browser.
mkCookieSessionExpired :: Environment -> Maybe Text -> Text
mkCookieSessionExpired env mDomain =
  fold $
    [ sessionCookieNameText env,
      "=; ",
      "SameSite=lax",
      "; ",
      "Path=/",
      "; ",
      "HttpOnly",
      "; ",
      "Secure",
      "; ",
      "Max-Age=0"
    ]
      <> maybe [] (\domain -> ["; ", "Domain=", domain]) mDomain
