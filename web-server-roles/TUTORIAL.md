# web-server-roles Tutorial

`web-server-roles` provides a `RequireRole` Servant combinator for declarative, type-level role-based access control. Instead of checking permissions inside your handlers, you declare the required role directly in your API type. The combinator handles authentication and authorization in a single step, returning 401 for unauthenticated requests and 403 for insufficient permissions.

## Prerequisites

This tutorial assumes familiarity with:

- Servant API types and `HasServer`
- `AuthProtect` and `AuthHandler` from `servant-server`
- The `AuthServerData` type family
- `DataKinds` (role names and auth tags are type-level `Symbol`s)

## Step 1: Define Your Role Type

Define your role type, deriving `Generic` (so the combinator can turn a role *name* into its value) and, for a linear hierarchy, `Ord` (so constructors listed later represent more permissions):

```haskell
data UserRole = User | Host | Staff | Admin
  deriving (Eq, Ord, Show, Generic)
```

With this ordering, `Admin >= Staff >= Host >= User`.

## Step 2: Write a RoleCheck Instance

`RoleCheck` defines how roles are compared. For a linear hierarchy, an empty instance uses the `Ord`-based default, where a role is sufficient when `actual >= required`:

```haskell
import App.Auth.Role (RoleCheck)

instance RoleCheck UserRole
```

The hierarchy is defined  by your `Ord` instance. For non-hierarchical schemes (permission sets, exact match), override `sufficient` — see [Alternative: Permission Sets](#alternative-permission-sets).

## Step 3: Write a HasRole Instance

`HasRole` tells the combinator how to extract a role from your authentication result type:

```haskell
import App.Auth.Role (HasRole(..))

instance HasRole Authz where
  type RoleOf Authz = UserRole
  getRole authz = authz.authzUser.userRole
```

## Step 4: Set Up AuthServerData

If you're already using `AuthProtect`, you'll have an `AuthServerData` type instance mapping your auth tag to your auth result type:

```haskell
type instance AuthServerData (AuthProtect "cookie-auth") = Authz
```

`RequireRole` reuses this same mapping. It looks up the `AuthHandler` from Servant's context using the tag, runs it, and then checks the role on the result.

## Step 5: Use RequireRole in Your API

`RequireRole` replaces `AuthProtect` in routes where you need role checking. It takes two type arguments: the auth tag (a `Symbol`, same as `AuthProtect`) and the required role named by its constructor name (also a `Symbol`):

```haskell
-- Role-protected route:
type DashboardAPI =
  RequireRole "cookie-auth" "Host"
    :> "dashboard"
    :> Get '[HTML] (Html ())

-- Auth-only route (no role check, keep using AuthProtect):
type ProfileAPI =
  AuthProtect "cookie-auth"
    :> "profile"
    :> Get '[HTML] (Html ())
```

The role name is checked at compile time. `RequireRole "cookie-auth" "Hsot"` (a typo) is a type error (`RequireRole: no constructor "Hsot" in role type UserRole`).

Your handler receives the auth result as its first argument, just like `AuthProtect`:

```haskell
dashboardHandler :: Authz -> Handler (Html ())
dashboardHandler authz = do
  let user = authz.authzUser
  -- the "Host" check already passed at this point
  pure (renderDashboard user)
```

## Step 6: Wire Up the Server

The Servant context is identical to what you'd use with `AuthProtect`, an `AuthHandler` keyed by the same tag:

```haskell
import Servant.Server (Context(..), serveWithContext)
import Servant.Server.Experimental.Auth (AuthHandler)

type API = DashboardAPI :<|> ProfileAPI

server :: Server API
server = dashboardHandler :<|> profileHandler

app :: Application
app =
  serveWithContext
    (Proxy @API)
    (myAuthHandler :. EmptyContext)
    server
```

Both `RequireRole "cookie-auth" "Host"` and `AuthProtect "cookie-auth"` look up the same `AuthHandler Request Authz` from the context. No extra context entries are needed.

## How It Works

When a request hits a `RequireRole` route:

1. The `AuthHandler` from the Servant context is invoked with the WAI `Request`.
2. If the auth handler throws an error (e.g. 401), that error is returned to the client immediately.
3. If authentication succeeds, the role is extracted from the result via `getRole`.
4. The required role name is demoted to a value of your role type (at compile time, via `Generic`) and `sufficient required actual` is evaluated.
5. If `sufficient` returns `True`, the auth result is passed to the handler.
6. If `sufficient` returns `False` Servant tries the next `:<|>` alternative.

All of this happens inside a single `addAuthCheck` in Servant's `Delayed` pipeline. There is no double authentication or extra middleware.

## Route Alternatives (Same Path, Different Roles)

Because role failures are non-fatal, you can serve different handlers for different role levels on the same path:

```haskell
type API =
       RequireRole "cookie-auth" "Admin" :> "panel" :> Get '[JSON] AdminView
  :<|> RequireRole "cookie-auth" "Host"  :> "panel" :> Get '[JSON] HostView
  :<|> RequireRole "cookie-auth" "User"  :> "panel" :> Get '[JSON] UserView
```

When a `Host` requests `/panel`:
1. The `"Admin"` route fails the role check (non-fatal) and Servant tries the next alternative.
2. The `"Host"` route passes so that handler runs.

Important: With the `>=` default, routes must be ordered most-restrictive-first. A `"User"` route (which every role satisfies) placed first would make more restrictive routes unreachable.

**Note on re-authentication:** Each `RequireRole` alternative runs the auth handler independently. In the example above, a `Host` request runs the auth handler twice (once for the `"Admin"` route that fails, once for the `"Host"` route that succeeds). Keep this in mind if your auth handler is expensive.

## Mixing RequireRole and AuthProtect

You can freely mix both combinators in the same API. They share the same `AuthHandler` in the context:

```haskell
type API =
       RequireRole "cookie-auth" "Admin" :> "admin" :> Get '[JSON] AdminData
  :<|> RequireRole "cookie-auth" "Host"  :> "dashboard" :> Get '[JSON] Dashboard
  :<|> AuthProtect "cookie-auth"         :> "profile" :> Get '[JSON] Profile
  :<|> "public" :> Get '[JSON] PublicInfo
```

- `/admin` requires `Admin` role or higher
- `/dashboard` requires `Host` role or higher
- `/profile` requires authentication only (any role)
- `/public` has no authentication

## Alternative: Permission Sets

`RoleCheck` is not limited to linear hierarchies. Override `sufficient` (and, when the required value has a different type than the role, the `Required` associated type) for any scheme. For example, set membership:

```haskell
import App.Auth.Role (RoleCheck(..))
import Data.Set (Set)
import qualified Data.Set as Set

data Permission = CanRead | CanEdit | CanDelete
  deriving (Eq, Ord, Show, Generic)

-- The role is a Set of permissions; the *required* value is a single Permission.
instance RoleCheck (Set Permission) where
  type Required (Set Permission) = Permission
  sufficient p perms = p `Set.member` perms
```

Your auth type's `RoleOf` would be `Set Permission`, and routes name a single permission constructor:

```haskell
type EditAPI = RequireRole "cookie-auth" "CanEdit" :> "edit" :> Post '[JSON] ()
```

This works with the same `RequireRole` combinator. Instances whose head is not a plain data type, like `RoleCheck (Set Permission)`, need `FlexibleInstances`.

## Complete Minimal Example

A self-contained example you can adapt:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import App.Auth.Role (HasRole(..), RequireRole, RoleCheck)
import Control.Monad.Except (throwError)
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import Network.Wai (Request, requestHeaders)
import Network.Wai.Handler.Warp (run)
import Servant.API (Get, JSON, type (:<|>) (..), type (:>))
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server (Context(..), Handler, Server, err401, serveWithContext)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)

-- Role type: Ord encodes the hierarchy, Generic enables name -> value demotion
data Role = Member | Moderator | Admin
  deriving (Eq, Ord, Show, Generic)

-- Comparison, defined once: the Ord default means "actual >= required"
instance RoleCheck Role

-- Auth result type
data Auth = Auth { authUser :: String, authRole :: Role }

instance HasRole Auth where
  type RoleOf Auth = Role
  getRole = authRole

-- Map the auth tag to the auth result type
type instance AuthServerData (AuthProtect "my-auth") = Auth

-- Auth handler (replace with real logic)
myAuthHandler :: AuthHandler Request Auth
myAuthHandler = mkAuthHandler $ \req ->
  case lookup "X-Role" (requestHeaders req) of
    Just "admin" -> pure (Auth "alice" Admin)
    Just "mod"   -> pure (Auth "bob" Moderator)
    Just _       -> pure (Auth "carol" Member)
    Nothing      -> throwError err401

-- API: same path, different handlers per role level (most restrictive first).
-- The required role is its constructor NAME; a typo is a compile error.
type API =
       RequireRole "my-auth" "Admin"     :> "panel" :> Get '[JSON] String
  :<|> RequireRole "my-auth" "Moderator" :> "panel" :> Get '[JSON] String
  :<|> RequireRole "my-auth" "Member"    :> "panel" :> Get '[JSON] String

server :: Server API
server = adminPanel :<|> modPanel :<|> memberPanel
  where
    adminPanel auth   = pure $ "Admin panel for " <> authUser auth
    modPanel auth     = pure $ "Mod panel for " <> authUser auth
    memberPanel auth  = pure $ "Member panel for " <> authUser auth

main :: IO ()
main = run 3000 $
  serveWithContext (Proxy @API) (myAuthHandler :. EmptyContext) server
```

```
$ curl -H "X-Role: admin" localhost:3000/panel
"Admin panel for alice"

$ curl -H "X-Role: mod" localhost:3000/panel
"Mod panel for bob"

$ curl -H "X-Role: member" localhost:3000/panel
"Member panel for carol"

$ curl localhost:3000/panel
401 Unauthorized
```

## Required Extensions and Dependencies

**GHC extensions** (most are covered by `GHC2021`):

- `DataKinds` — role names and auth tags are type-level `Symbol`s
- `DeriveGeneric` — to `deriving Generic` on your role type
- `TypeFamilies` — for `RoleOf`, `Required`, and `AuthServerData`
- `TypeOperators` — for `:>`
- `FlexibleInstances` — only if a `RoleCheck`/`HasRole` instance head is not a plain data type (e.g. `RoleCheck (Set Permission)`)

Your role type must derive `Generic`. The library's own internals (`PolyKinds`, `AllowAmbiguousTypes`, `DefaultSignatures`, etc.) do not leak into your code.

**Cabal dependencies**:

- `web-server-roles`
- `servant` and `servant-server` (you likely already have these)
