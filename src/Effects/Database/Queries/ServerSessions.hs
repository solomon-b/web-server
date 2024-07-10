module Effects.Database.Queries.ServerSessions where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadThrow)
import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Types.ServerSessions (ServerSession, SessionId (..))
import Effects.Database.Class (MonadDB)
import Effects.Database.Tables.ServerSessions qualified as ServerSessions
import Effects.Database.Tables.User qualified as User
import Effects.Database.Utils
import Hasql.Pool qualified as HSQL
import Hasql.Statement qualified as HSQL
import Log qualified
import Network.IP.Addr (IP, NetAddr)
import Rel8 ((&&.), (<.), (==.))
import Rel8 qualified
import Rel8.Expr.Time qualified as Rel8.Time

--------------------------------------------------------------------------------

-- selectServerSession ::
--   ( Log.MonadLog f,
--     MonadDB f,
--     MonadThrow f
--   ) =>
--   ServerSessions.Id ->
--   f (Maybe (User, ServerSession))
-- selectServerSession sid =
--   execQuerySpanThrowMessage' "Failed to query users table" (selectServerSessionQuery sid)

selectServerSessionQuery :: ServerSessions.Id -> HSQL.Statement () (Maybe (User.Model Rel8.Result, ServerSessions.Model Rel8.Result))
selectServerSessionQuery sid = Rel8.runMaybe . Rel8.select $ do
  sm <- Rel8.each ServerSessions.schema
  um <- Rel8.each User.schema
  Rel8.where_ $
    foldr1 (&&.) [ServerSessions.ssId sm ==. Rel8.litExpr sid, ServerSessions.ssUserId sm ==. User.umId um, Rel8.Time.now <. ServerSessions.ssExpiresAt sm]
  pure (um, sm)

selectServerSessionByUser ::
  ( Log.MonadLog f,
    MonadDB f,
    MonadThrow f
  ) =>
  User.Id ->
  f (Maybe ServerSession)
selectServerSessionByUser uid =
  execQuerySpanThrowMessage' "Failed to query users table" (selectServerSessionByUserQuery uid)

selectServerSessionByUserQuery :: User.Id -> HSQL.Statement () (Maybe (ServerSessions.Model Rel8.Result))
selectServerSessionByUserQuery uid = Rel8.runMaybe . Rel8.select $ do
  sm <- Rel8.each ServerSessions.schema
  Rel8.where_ $
    ServerSessions.ssUserId sm ==. Rel8.litExpr uid &&. Rel8.Time.now <. ServerSessions.ssExpiresAt sm
  pure sm

insertServerSession ::
  ( Log.MonadLog f,
    MonadDB f
  ) =>
  (User.Id, Maybe (NetAddr IP), Maybe Text, UTCTime) ->
  f (Either HSQL.UsageError SessionId)
insertServerSession session = do
  coerce <$> execQuerySpan (insertServerSessionQuery session)

insertServerSessionQuery ::
  (User.Id, Maybe (NetAddr IP), Maybe Text, UTCTime) -> HSQL.Statement () ServerSessions.Id
insertServerSessionQuery session =
  Rel8.run1 $
    Rel8.insert $
      Rel8.Insert
        { into = ServerSessions.schema,
          rows = Rel8.values [printModel session],
          onConflict = Rel8.Abort,
          returning = Rel8.Returning ServerSessions.ssId
        }

expireServerSession ::
  ( Log.MonadLog f,
    MonadDB f
  ) =>
  ServerSessions.Id ->
  f (Either HSQL.UsageError ())
expireServerSession sid = execQuerySpan (expireServerSessionQuery sid)

expireServerSessionQuery :: ServerSessions.Id -> HSQL.Statement () ()
expireServerSessionQuery sid =
  Rel8.run_ $
    Rel8.update $
      Rel8.Update
        { target = ServerSessions.schema,
          from = pure (),
          set = \() ss -> ss {ServerSessions.ssExpiresAt = Rel8.Time.now},
          updateWhere = \_ ss -> ServerSessions.ssId ss ==. Rel8.litExpr sid,
          returning = Rel8.NoReturning
        }
