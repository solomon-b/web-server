{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Auth where

--------------------------------------------------------------------------------

import Control.Error (note)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Functor ((<&>))
import Data.IP (IP (..), IPRange (..), fromSockAddr, makeAddrRange)
import Data.Text (Text)
import Effects.Clock (MonadClock)
import Effects.Clock qualified as Clock
import Effects.Database.Class (MonadDB (..), execStatement)
import Effects.Database.Tables.ServerSessions qualified as Session
import Effects.Database.Tables.User qualified as User
import Errors (InternalServerError (..), ToServerError (..), throwErr, toErrorBody)
import Hasql.Interpolate (getOneRow)
import Hasql.Pool qualified as HSQL
import Hasql.Session qualified as HSQL
import Log qualified
import Network.Socket (SockAddr)
import Network.Wai (Request (..))
import Servant qualified
import Servant.Server.Experimental.Auth (mkAuthHandler)
import Servant.Server.Experimental.Auth qualified as Servant
import Web.Cookie (parseCookies)

--------------------------------------------------------------------------------

type instance Servant.AuthServerData (Servant.AuthProtect "cookie-auth") = Authz

data Authz = Authz
  { authzUser :: User.Domain,
    authzSession :: Session.Domain
  }

data AuthErr
  = MissingCookieHeader
  | MissingCookieValue
  | MalformedSessionId
  deriving (Show)

instance ToServerError AuthErr where
  toServerError = \case
    MissingCookieHeader -> Servant.err307 {Servant.errBody = toErrorBody "No cookie sent in request" 307, Servant.errHeaders = [("Location", "/login")]}
    MissingCookieValue -> Servant.err307 {Servant.errBody = toErrorBody "Invalid cookie" 307, Servant.errHeaders = [("Location", "/login")]}
    MalformedSessionId -> Servant.err307 {Servant.errBody = toErrorBody "Bad session data" 307, Servant.errHeaders = [("Location", "/login")]}

authHandler :: HSQL.Pool -> Servant.AuthHandler Request Authz
authHandler pool = mkAuthHandler $ \req ->
  let eSession = do
        cookie <- note MissingCookieHeader $ lookup "cookie" $ requestHeaders req
        sessionId <- note MissingCookieValue $ lookup "session-id" $ parseCookies cookie
        note MalformedSessionId $ Session.parseSessionId sessionId
   in case eSession of
        Right sessionId ->
          liftIO (HSQL.use pool (HSQL.statement () $ Session.getSessionUser sessionId)) >>= \case
            Left _err -> do
              -- TODO: Log censored error here?
              throwErr InternalServerError
            Right Nothing ->
              throwErr $ Servant.err307 {Servant.errBody = "forbidden", Servant.errHeaders = [("Location", "/login")]}
            Right (Just (userModel, sessionModel)) ->
              pure $ Authz (User.toDomain userModel) (Session.toDomain sessionModel)
        Left err -> throwErr err

login ::
  (MonadDB m, MonadClock m, Log.MonadLog m) => User.Id -> SockAddr -> Maybe Text -> m (Either HSQL.UsageError Session.Id)
login uid sockAddr mUserAgent = do
  now <- Clock.currentSystemTime
  let sessionExpiration = Clock.addUTCTime Clock.nominalDay now
      sessionToInsert = Session.ServerSessionInsert uid (mkIpRange sockAddr) mUserAgent sessionExpiration
  fmap (fmap (Session.mSessionId . getOneRow)) $ execStatement $ Session.insertServerSession sessionToInsert

mkIpRange :: SockAddr -> Maybe IPRange
mkIpRange sockAddr =
  fromSockAddr sockAddr <&> \case
    (IPv4 ip, _) -> IPv4Range $ makeAddrRange ip 0
    (IPv6 ip, _) -> IPv6Range $ makeAddrRange ip 0

expireSession ::
  (MonadDB m, Log.MonadLog m) =>
  Session.Id ->
  m (Either HSQL.UsageError ())
expireSession = execStatement . Session.expireSession
