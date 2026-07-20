module API.User.Logout.Post
  ( Route,
    handler,
  )
where

import App.Auth qualified as Auth
import App.Config (Environment)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Data.Text qualified as Text
import Hasql.Pool qualified as HSQL
import Log qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | Logout is deliberately /not/ behind the auth handler. A session that has
-- passed the idle timeout no longer authenticates, so gating logout on a valid
-- session would make it impossible to log out of exactly the sessions most in
-- need of it — the browser would keep its dead cookie with no way to shed it.
-- Instead we read the cookie directly and always respond with the clearing
-- @Set-Cookie@.
type Route =
  Servant.Header "Cookie" Text
    :> "user"
    :> "logout"
    :> Servant.Post
         '[HTML]
         ( Servant.Headers
             '[ Servant.Header "Location" Text,
                Servant.Header "Set-Cookie" Text
              ]
             Servant.NoContent
         )

--------------------------------------------------------------------------------

-- | Expire the server-side session named by the cookie, if there is one, and
-- clear the browser cookie unconditionally. Every outcome — no cookie, an
-- unparseable cookie, an already-expired session, or a database failure —
-- produces the same redirect-and-clear response, so logout is idempotent and
-- cannot fail from the caller's point of view.
handler ::
  ( Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool env,
    MonadIO m
  ) =>
  Environment ->
  Maybe Text ->
  m
    ( Servant.Headers
        '[ Servant.Header "Location" Text,
           Servant.Header "Set-Cookie" Text
         ]
        Servant.NoContent
    )
handler env mCookie = do
  case mCookie >>= Auth.lookupSessionId env of
    Nothing ->
      pure ()
    Just sessionId ->
      Auth.expireServerSession sessionId >>= \case
        -- The cookie is cleared regardless, but the server-side session
        -- outlived the logout. That is worth surfacing rather than failing the
        -- request, which would strand the user with a cookie they cannot shed.
        Left err ->
          Log.logAttention_ $
            "Logout failed to expire session "
              <> Auth.truncateSessionId sessionId
              <> ": "
              <> Text.pack (show err)
        Right () ->
          pure ()
  pure $ Servant.addHeader "/" $ Servant.addHeader (Auth.mkCookieSessionExpired env Nothing) Servant.NoContent
