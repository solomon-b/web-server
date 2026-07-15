-- | Type-level role-based authorization for Servant.
--
-- This module provides the 'RequireRole' combinator for declarative
-- role-based access control in Servant APIs.
--
-- == Quick Start
--
-- 1. Define your role type, deriving 'Generic' (and usually 'Ord' for a
--    linear hierarchy):
--
-- @
-- data UserRole = User | Host | Staff | Admin
--   deriving (Eq, Ord, Show, Generic)
-- @
--
-- 2. Declare how roles are compared. For a linear hierarchy, an empty
--    'RoleCheck' instance uses the 'Ord'-based default (@actual >= required@):
--
-- @
-- instance RoleCheck UserRole
-- @
--
-- 3. Tell the combinator how to extract the role from your auth type:
--
-- @
-- instance HasRole Authz where
--   type RoleOf Authz = UserRole
--   getRole = userRole . authzUser
-- @
--
-- 4. Use 'RequireRole' in your API, naming the required role by its
--    __constructor name__ (a checked 'Symbol' — a typo is a compile error):
--
-- @
-- type API = RequireRole "cookie-auth" "Host"
--        :> "dashboard"
--        :> Get '[HTML] (Html ())
-- @
--
-- The handler runs only if 'sufficient' holds for the authenticated user's
-- role. Otherwise a 403 Forbidden response is returned.
module App.Auth.Role
  ( -- * Combinator
    RequireRole,

    -- * Type Classes
    RoleCheck (..),
    HasRole (..),
  )
where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (liftIO)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.Type.Bool (type (||))
import GHC.Generics
import GHC.TypeLits
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
-- It replaces 'AuthProtect' in routes. It takes a @tag@ (matching the tag you
-- would use with 'AuthProtect') and a required role named by its __constructor
-- name__ as a 'Symbol'. The auth type is resolved via the 'AuthServerData' type
-- family; the role type via 'RoleOf'.
--
-- @
-- type ProtectedRoute = RequireRole "cookie-auth" "Admin" :> "admin" :> Get '[JSON] AdminData
-- @
--
-- The role name is demoted to a value of your role type at compile time using
-- 'Generic'; naming a constructor that does not exist is a type error. The
-- combinator then calls 'sufficient' to decide whether the authenticated user's
-- role satisfies the requirement.
--
-- == Route Alternatives
--
-- Role check failures are __non-fatal__: Servant tries the next @(':<|>')@
-- alternative when the role check fails. This lets you serve different handlers
-- for different role levels on the same path:
--
-- @
-- type API
--   =    RequireRole "cookie-auth" "Admin"  :> "panel" :> Get '[JSON] AdminView
--   :\<|> RequireRole "cookie-auth" "Member" :> "panel" :> Get '[JSON] MemberView
-- @
--
-- __Important:__ with the @>=@ default in 'sufficient', routes must be ordered
-- most-restrictive-first, or a less-restrictive route will shadow a more
-- restrictive one.
--
-- Authentication failures (missing or invalid credentials) are __fatal__ and
-- immediately return the auth handler's error without trying alternatives.
data RequireRole (tag :: Symbol) (roleName :: Symbol)

--------------------------------------------------------------------------------
-- Type Classes

-- | Extract a role value from an authentication result.
--
-- @
-- instance HasRole Authz where
--   type RoleOf Authz = UserRole
--   getRole = userRole . authzUser
-- @
class HasRole auth where
  -- | The role type carried by this auth type.
  type RoleOf auth :: Type

  getRole :: auth -> RoleOf auth

-- | How roles are compared, defined __once per role type__ (not per role).
--
-- For a linear hierarchy, an empty instance uses the 'Ord'-based default,
-- where @'sufficient' required actual = actual >= required@:
--
-- @
-- instance RoleCheck UserRole
-- @
--
-- For non-hierarchical schemes, override 'sufficient' (and, if the required
-- value has a different type than the role, 'Required'). For example,
-- permission sets:
--
-- @
-- instance RoleCheck (Set Permission) where
--   type Required (Set Permission) = Permission
--   sufficient p perms = p \`Set.member\` perms
-- @
class RoleCheck a where
  -- | The type of the /required/ value. Defaults to @a@ itself (the
  -- hierarchical case). Override when the requirement is expressed by a
  -- different type than the role (e.g. one 'Permission' against a @Set@).
  type Required a :: Type

  type Required a = a

  -- | Does @actual@ satisfy @required@?
  sufficient :: Required a -> a -> Bool
  default sufficient :: (Required a ~ a, Ord a) => Required a -> a -> Bool
  sufficient required actual = actual >= required

--------------------------------------------------------------------------------
-- Generic demotion: a constructor name ('Symbol') to its value
--
-- Pure GHC.Generics cannot demote a promoted constructor, but it can build the
-- value for a constructor named by a Symbol, with a compile-time check that the
-- constructor exists.

type family SymEq (a :: Symbol) (b :: Symbol) :: Bool where
  SymEq a a = 'True
  SymEq _ _ = 'False

-- | Does the generic representation contain a nullary constructor named @name@?
type family HasCtor (name :: Symbol) (f :: Type -> Type) :: Bool where
  HasCtor name (D1 _ f) = HasCtor name f
  HasCtor name (l :+: r) = HasCtor name l || HasCtor name r
  HasCtor name (C1 ('MetaCons cn _ _) _) = SymEq name cn
  HasCtor _ _ = 'False

type family CheckHas (name :: Symbol) (a :: Type) (b :: Bool) :: Constraint where
  CheckHas _ _ 'True = ()
  CheckHas name a 'False =
    TypeError
      ( 'Text "RequireRole: no constructor "
          ':<>: 'ShowType name
          ':<>: 'Text " in role type "
          ':<>: 'ShowType a
      )

class GDemote (name :: Symbol) (f :: Type -> Type) where
  gdemote :: f x

instance (GDemote name f) => GDemote name (D1 meta f) where
  gdemote = M1 (gdemote @name)

instance (GDemoteSum (HasCtor name l) name l r) => GDemote name (l :+: r) where
  gdemote = gdemoteSum @(HasCtor name l) @name

instance GDemote name (C1 ('MetaCons cn fx sel) U1) where
  gdemote = M1 U1

class GDemoteSum (leftHas :: Bool) (name :: Symbol) (l :: Type -> Type) (r :: Type -> Type) where
  gdemoteSum :: (l :+: r) x

instance (GDemote name l) => GDemoteSum 'True name l r where
  gdemoteSum = L1 (gdemote @name)

instance (GDemote name r) => GDemoteSum 'False name l r where
  gdemoteSum = R1 (gdemote @name)

-- | Everything needed to demote constructor @name@ of type @a@.
type DemoteRole (name :: Symbol) (a :: Type) =
  (Generic a, GDemote name (Rep a), CheckHas name a (HasCtor name (Rep a)))

-- | Demote constructor @name@ of enum @a@ to its value. A missing constructor
-- is a compile-time 'TypeError'.
demoteCtor :: forall (name :: Symbol) a. (DemoteRole name a) => a
demoteCtor = to (gdemote @name @(Rep a))

--------------------------------------------------------------------------------
-- HasServer Instance

instance
  forall tag roleName api context.
  ( HasServer api context,
    HasRole (AuthServerData (AuthProtect tag)),
    RoleCheck (RoleOf (AuthServerData (AuthProtect tag))),
    DemoteRole roleName (Required (RoleOf (AuthServerData (AuthProtect tag)))),
    HasContextEntry context (AuthHandler Request (AuthServerData (AuthProtect tag)))
  ) =>
  HasServer (RequireRole tag roleName :> api) context
  where
  type
    ServerT (RequireRole tag roleName :> api) m =
      AuthServerData (AuthProtect tag) -> ServerT api m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy @api) pc nt . s

  route _ context subserver =
    route (Proxy @api) context (subserver `addAuthCheck` withRequest authCheck)
    where
      authHandler' :: Request -> Handler (AuthServerData (AuthProtect tag))
      authHandler' = unAuthHandler (getContextEntry context)

      required :: Required (RoleOf (AuthServerData (AuthProtect tag)))
      required = demoteCtor @roleName

      authCheck :: Request -> DelayedIO (AuthServerData (AuthProtect tag))
      authCheck req = do
        eResult <- liftIO $ runHandler (authHandler' req)
        case eResult of
          Left err -> delayedFailFatal err
          Right auth ->
            if sufficient required (getRole auth)
              then pure auth
              else
                delayedFail
                  err403
                    { errBody = "Forbidden: insufficient permissions",
                      errHeaders = [("Content-Type", "text/plain; charset=utf-8")]
                    }
