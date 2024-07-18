{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Auth where

--------------------------------------------------------------------------------

import Auth.Network (sockAddrToNetAddr)
import Control.Error (note)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Coerce
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.Time.Clock qualified as Clock
import Domain.Types.ServerSessions (ServerSession, SessionId (..), parseSessionId)
import Domain.Types.User (User)
import Effects.Database.Class (MonadDB)
import Effects.Database.Queries.ServerSessions (expireServerSession, insertServerSession, selectServerSessionQuery)
import Effects.Database.Tables.ServerSessions qualified as Session
import Effects.Database.Tables.User qualified as User
import Effects.Database.Utils
import Errors (InternalServerError (..), ToServerError (..), throwErr, toErrorBody)
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
  { authzUser :: User,
    authzSession :: ServerSession
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
        note MalformedSessionId $ parseSessionId sessionId
   in case eSession of
        Right sessionId ->
          liftIO (HSQL.use pool (HSQL.statement () $ selectServerSessionQuery $ coerce sessionId)) >>= \case
            Left _err -> do
              -- TODO: Log censored error here?
              throwErr InternalServerError
            Right Nothing ->
              throwErr $ Servant.err307 {Servant.errBody = "forbidden", Servant.errHeaders = [("Location", "/login")]}
            Right (Just (userModel, sessionModel)) ->
              pure $ Authz (parseModel userModel) (parseModel sessionModel)
        Left err -> throwErr err

login ::
  (MonadDB m, Log.MonadLog m) => User.Id -> SockAddr -> Maybe Text -> m (Either HSQL.UsageError SessionId)
login uid sockAddr mUserAgent = do
  -- TODO: Add clock effect:
  now <- liftIO getCurrentTime
  let sessionExpiration = Clock.addUTCTime Clock.nominalDay now
      sessionToInsert = (uid, sockAddrToNetAddr sockAddr, mUserAgent, sessionExpiration)
  insertServerSession sessionToInsert

expireSession ::
  (MonadDB m, Log.MonadLog m) =>
  SessionId ->
  m (Either HSQL.UsageError ())
expireSession = expireServerSession . coerce
