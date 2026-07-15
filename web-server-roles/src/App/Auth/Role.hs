-- | Type-level role-based authorization for Servant.
--
-- This module provides the 'RequireRole' combinator for declarative
-- role-based access control in Servant APIs.
--
-- == Quick Start
--
-- 1. Define your role type and a 'CheckRole' instance for each constructor.
--    'checkRole' receives the user's actual role and returns whether it
--    is sufficient for the required role:
--
-- @
-- data UserRole = User | Host | Staff | Admin
--   deriving (Eq, Ord, Show)
--
-- instance CheckRole 'User  where type RoleType 'User  = UserRole; checkRole _ role = role >= User
-- instance CheckRole 'Host  where type RoleType 'Host  = UserRole; checkRole _ role = role >= Host
-- instance CheckRole 'Staff where type RoleType 'Staff = UserRole; checkRole _ role = role >= Staff
-- instance CheckRole 'Admin where type RoleType 'Admin = UserRole; checkRole _ role = role >= Admin
-- @
--
-- 2. Provide a 'HasRole' instance to extract the role from your auth type:
--
-- @
-- instance HasRole Authz UserRole where
--   getRole = userRole . authzUser
-- @
--
-- 3. Use 'RequireRole' in your API (it replaces 'AuthProtect'):
--
-- @
-- type API = RequireRole "cookie-auth" 'Host
--        :> "dashboard"
--        :> Get '[HTML] (Html ())
-- @
--
-- The handler will only run if @checkRole \@\'Host@ returns 'True' for the
-- authenticated user's role. Otherwise, a 403 Forbidden response is returned.
module App.Auth.Role
  ( -- * Combinator
    RequireRole,

    -- * Type Classes
    CheckRole (..),
    HasRole (..),

    -- * Re-exports for convenience
    Proxy (..),
  )
where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (liftIO)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (Symbol)
import Network.Wai (Request)
import Servant.API (type (:>))
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server
  ( Handler,
    HasContextEntry (..),
    HasServer (..),
    ServerError (..),
    err403,
  )
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, unAuthHandler)
import Servant.Server.Internal.Delayed (addAuthCheck)
import Servant.Server.Internal.DelayedIO (DelayedIO, delayedFail, delayedFailFatal, withRequest)
import Servant.Server.Internal.Handler (runHandler)

--------------------------------------------------------------------------------
-- Combinator

-- | A Servant combinator that performs authentication and requires a role check.
--
-- This combinator replaces 'AuthProtect' in routes. It takes a @tag@ (matching
-- the tag you would use with 'AuthProtect') and a role @r@. The auth type is
-- resolved via the 'AuthServerData' type family.
--
-- @
-- type ProtectedRoute = RequireRole "cookie-auth" 'Admin :> "admin" :> Get '[JSON] AdminData
-- @
--
-- The combinator calls 'checkRole' to determine whether the authenticated
-- user's role is sufficient. The logic is entirely defined by your 'CheckRole'
-- instances — you can use 'Ord', set membership, or any custom predicate.
--
-- == Route Alternatives
--
-- Role check failures are __non-fatal__, meaning Servant will try the next
-- @(':<|>')@ alternative when the role check fails. This lets you serve
-- different handlers for different role levels on the same path:
--
-- @
-- type API
--   =    RequireRole "cookie-auth" 'Admin  :> "panel" :> Get '[JSON] AdminView
--   :\<|> RequireRole "cookie-auth" 'Member :> "panel" :> Get '[JSON] MemberView
-- @
--
-- __Important:__ when using @>=@ in 'checkRole', routes must be ordered
-- most-restrictive-first. A @'Member@ route whose 'checkRole' uses @>=@
-- would match @Member@, @Host@, @Staff@, /and/ @Admin@, so placing it
-- before a more restrictive route would make the latter unreachable.
--
-- Authentication failures (e.g., missing or invalid credentials) are __fatal__
-- and immediately return the auth handler's error without trying alternatives.
--
-- __Note on re-authentication:__ each @(':<|>')@ alternative runs the
-- 'AuthHandler' independently. In the example above, a @Member@ request
-- runs the auth handler twice — once for the @\'Admin@ route that fails
-- the role check, and once for the @\'Member@ route that succeeds. Keep
-- this in mind if your auth handler is expensive (e.g., database lookups).
data RequireRole (tag :: Symbol) (r :: k)

--------------------------------------------------------------------------------
-- Type Classes

-- | Role check class that determines whether a user's role satisfies a
-- type-level role requirement.
--
-- Each instance defines 'checkRole', which receives the user's actual role
-- and returns whether it is sufficient for the required role @r@.
--
-- === Hierarchical roles (using 'Ord')
--
-- @
-- data UserRole = User | Host | Staff | Admin
--   deriving (Eq, Ord, Show)
--
-- instance CheckRole 'User  where
--   type RoleType 'User = UserRole
--   checkRole _ role = role >= User
--
-- instance CheckRole 'Admin where
--   type RoleType 'Admin = UserRole
--   checkRole _ role = role >= Admin
-- @
--
-- === Permission-set roles
--
-- @
-- data Permission = CanRead | CanEdit | CanDelete
--   deriving (Eq, Ord, Show)
--
-- instance CheckRole 'CanEdit where
--   type RoleType 'CanEdit = Set Permission
--   checkRole _ perms = CanEdit \`Set.member\` perms
-- @
class CheckRole (r :: k) where
  -- | The value-level type that this role checks against.
  type RoleType r :: Type

  -- | Check whether the given role value satisfies the requirement @r@.
  checkRole :: Proxy r -> RoleType r -> Bool

-- | Extract a role from an authentication context.
--
-- Implement this for your auth type (e.g., @Authz@) to tell the combinator
-- how to get the user's role:
--
-- @
-- instance HasRole Authz UserRole where
--   getRole authz = authz.authzUser.userRole
-- @
class HasRole auth r | auth -> r where
  getRole :: auth -> r

--------------------------------------------------------------------------------
-- HasServer Instance

instance
  forall tag r api context.
  ( HasServer api context,
    CheckRole r,
    HasRole (AuthServerData (AuthProtect tag)) (RoleType r),
    HasContextEntry context (AuthHandler Request (AuthServerData (AuthProtect tag)))
  ) =>
  HasServer (RequireRole tag r :> api) context
  where
  type
    ServerT (RequireRole tag r :> api) m =
      AuthServerData (AuthProtect tag) -> ServerT api m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy @api) pc nt . s

  route _ context subserver =
    route (Proxy @api) context (subserver `addAuthCheck` withRequest authCheck)
    where
      authHandler' :: Request -> Handler (AuthServerData (AuthProtect tag))
      authHandler' = unAuthHandler (getContextEntry context)

      authCheck :: Request -> DelayedIO (AuthServerData (AuthProtect tag))
      authCheck req = do
        eResult <- liftIO $ runHandler (authHandler' req)
        case eResult of
          Left err -> delayedFailFatal err
          Right auth ->
            if checkRole (Proxy @r) (getRole auth)
              then pure auth
              else
                delayedFail
                  err403
                    { errBody = "Forbidden: insufficient permissions",
                      errHeaders = [("Content-Type", "text/plain; charset=utf-8")]
                    }
