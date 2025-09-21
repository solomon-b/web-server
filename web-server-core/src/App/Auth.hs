{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App.Auth where

--------------------------------------------------------------------------------

import App.Errors (InternalServerError (..), ToServerError (..), throwErr, toErrorBody)
import Control.Error (note)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader qualified as Reader
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.Foldable (fold)
import Data.Functor ((<&>))
import Data.Has (Has)
import Data.Has qualified as Has
import Data.IP (IP (..), IPRange (..), fromSockAddr, makeAddrRange)
import Data.Int (Int64)
import Data.Password.Argon2 (Argon2, PasswordHash)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display, display)
import Data.Text.Display.Generic (RecordInstance (..))
import Data.Text.Encoding qualified as Text.Encoding
import Data.Time (UTCTime, addUTCTime, getCurrentTime, nominalDay)
import Data.UUID (UUID, fromASCIIBytes)
import Domain.Types.EmailAddress (EmailAddress)
import Effects.Database.Tables.ServerSessions qualified as ServerSessions
import Effects.Database.Tables.User qualified as User
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue, EncodeRow, EncodeValue, OneRow, getOneRow, interp, sql)
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
      s.id as server_session_id, s.user_id as session_user_id, s.ip_address, s.user_agent, s.expires_at
    FROM server_sessions s
    JOIN users u ON u.id = s.user_id
    WHERE s.id = #{sId}
      AND NOW() < expires_at
  |]
  where
    fromRows ::
      ( User.Id,
        EmailAddress,
        PasswordHash Argon2,
        ServerSessions.Id,
        User.Id,
        Maybe IPRange,
        Maybe Text,
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
        mExpiresAt
        ) = (User.Model {..}, ServerSessions.Model {mUserId = sessionUserId, ..})

--------------------------------------------------------------------------------

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

authHandler :: HSQL.Pool -> Servant.AuthHandler Request Authz
authHandler pool = mkAuthHandler $ \req ->
  let eSession = do
        cookie <- note MissingCookieHeader $ lookup "cookie" $ requestHeaders req
        sessionId <- note MissingCookieValue $ lookup "session-id" $ parseCookies cookie
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
                -- Auth Success
              Right (Just (userModel, sessionModel)) ->
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
  fmap (fmap (ServerSessions.mSessionId . getOneRow)) $ execStatement $ ServerSessions.insertServerSession sessionToInsert

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
expireServerSession = execStatement . ServerSessions.expireSession

lookupSessionId :: Text -> Maybe ServerSessions.Id
lookupSessionId =
  lookup (Text.Encoding.encodeUtf8 "session-id") . parseCookies . Text.Encoding.encodeUtf8 >=> parseSessionId

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
  Maybe Text ->
  m LoggedIn
userLoginState cookie = do
  let mSessionId = cookie >>= lookupSessionId
  traverse getAuth mSessionId >>= \case
    Just (Right (Just Authz {..})) ->
      pure $ IsLoggedIn authzUser
    _ ->
      pure IsNotLoggedIn

mkCookieSession :: ServerSessions.Id -> Text
mkCookieSession sId =
  fold
    [ "session-id",
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
