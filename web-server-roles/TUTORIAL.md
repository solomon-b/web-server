# web-server-roles Tutorial

`web-server-roles` provides a `RequireRole` Servant combinator for declarative, type-level role-based access control. Instead of checking permissions inside your handlers, you declare the required role directly in your API type. The combinator handles authentication and authorization in a single step, returning 401 for unauthenticated requests and 403 for insufficient permissions.

## Prerequisites

This tutorial assumes familiarity with:

- Servant API types and `HasServer`
- `AuthProtect` and `AuthHandler` from `servant-server`
- The `AuthServerData` type family
- `DataKinds` for promoting constructors to the type level

## Step 1: Define Your Role Type

Define your role type. If you want a linear hierarchy, derive `Ord` so constructors listed later represent more permissions:

```haskell
data UserRole = User | Host | Staff | Admin
  deriving (Eq, Ord, Show)
```

With this ordering, `Admin >= Staff >= Host >= User`.

## Step 2: Write CheckRole Instances

`CheckRole` defines how each type-level role checks whether a user's actual role is sufficient. You need one instance per constructor, each implementing `checkRole`:

```haskell
import App.Auth.Role (CheckRole(..))

instance CheckRole 'User where
  type RoleType 'User = UserRole
  checkRole _ role = role >= User

instance CheckRole 'Host where
  type RoleType 'Host = UserRole
  checkRole _ role = role >= Host

instance CheckRole 'Staff where
  type RoleType 'Staff = UserRole
  checkRole _ role = role >= Staff

instance CheckRole 'Admin where
  type RoleType 'Admin = UserRole
  checkRole _ role = role >= Admin
```

Each instance declares `RoleType` (the value-level type to check against) and `checkRole` (a predicate that receives the user's actual role and returns whether it satisfies the requirement).

The logic inside `checkRole` is entirely up to you. Using `>=` with a derived `Ord` instance gives a linear hierarchy, but you can use any predicate — exact match, set membership, or custom logic. See [Alternative: Permission Sets](#alternative-permission-sets) below.

## Step 3: Write a HasRole Instance

`HasRole` tells the combinator how to extract a role from your authentication result type. If your `AuthHandler` returns an `Authz` that contains a `User` with a `userRole` field:

```haskell
import App.Auth.Role (HasRole(..))

instance HasRole Authz UserRole where
  getRole authz = authz.authzUser.userRole
```

The functional dependency `auth -> r` means each auth type maps to exactly one role type, so GHC can infer the role type automatically.

## Step 4: Set Up AuthServerData

If you're already using `AuthProtect`, you'll have an `AuthServerData` type instance mapping your auth tag to your auth result type:

```haskell
type instance AuthServerData (AuthProtect "cookie-auth") = Authz
```

`RequireRole` reuses this same mapping. It looks up the `AuthHandler` from Servant's context using the tag, runs it, and then checks the role on the result.

## Step 5: Use RequireRole in Your API

`RequireRole` **replaces** `AuthProtect` in routes where you need role checking. It takes two type arguments: the auth tag (a `Symbol`, same as `AuthProtect`) and the required role (a promoted constructor):

```haskell
-- Role-protected route:
type DashboardAPI =
  RequireRole "cookie-auth" 'Host
    :> "dashboard"
    :> Get '[HTML] (Html ())

-- Auth-only route (no role check, keep using AuthProtect):
type ProfileAPI =
  AuthProtect "cookie-auth"
    :> "profile"
    :> Get '[HTML] (Html ())
```

Your handler receives the auth result as its first argument, just like `AuthProtect`:

```haskell
dashboardHandler :: Authz -> Handler (Html ())
dashboardHandler authz = do
  let user = authz.authzUser
  -- checkRole @'Host already passed at this point
  pure (renderDashboard user)
```

## Step 6: Wire Up the Server

The Servant context is identical to what you'd use with `AuthProtect` — an `AuthHandler` keyed by the same tag:

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

Both `RequireRole "cookie-auth" 'Host` and `AuthProtect "cookie-auth"` look up the same `AuthHandler Request Authz` from the context. No extra context entries are needed.

## How It Works

When a request hits a `RequireRole` route:

1. The `AuthHandler` from the Servant context is invoked with the WAI `Request`.
2. If the auth handler throws an error (e.g. 401), that error is returned to the client **immediately** (fatal — no alternatives are tried).
3. If authentication succeeds, the role is extracted from the result via `getRole`.
4. `checkRole` is called with the user's actual role. The logic is defined by your `CheckRole` instance.
5. If `checkRole` returns `True`, the auth result is passed to the handler.
6. If `checkRole` returns `False`, the failure is **non-fatal** — Servant tries the next `:<|>` alternative.

All of this happens inside a single `addAuthCheck` in Servant's `Delayed` pipeline — there is no double authentication or extra middleware.

## Route Alternatives (Same Path, Different Roles)

Because role failures are non-fatal, you can serve different handlers for different role levels on the same path:

```haskell
type API =
       RequireRole "cookie-auth" 'Admin :> "panel" :> Get '[JSON] AdminView
  :<|> RequireRole "cookie-auth" 'Host  :> "panel" :> Get '[JSON] HostView
  :<|> RequireRole "cookie-auth" 'User  :> "panel" :> Get '[JSON] UserView
```

When a `Host` requests `/panel`:
1. The `'Admin` route fails `checkRole` (non-fatal) — Servant tries the next alternative.
2. The `'Host` route passes `checkRole` — that handler runs.

**Important:** When using `>=` in `checkRole`, routes must be ordered **most-restrictive-first**. A `'User` route whose `checkRole` uses `>=` would match `User`, `Host`, `Staff`, *and* `Admin`, so placing it before a more restrictive route would make the latter unreachable.

**Note on re-authentication:** Each `RequireRole` alternative runs the auth handler independently. In the example above, a `Host` request runs the auth handler twice (once for the `'Admin` route that fails, once for the `'Host` route that succeeds). Keep this in mind if your auth handler is expensive.

## Mixing RequireRole and AuthProtect

You can freely mix both combinators in the same API. They share the same `AuthHandler` in the context:

```haskell
type API =
       RequireRole "cookie-auth" 'Admin :> "admin" :> Get '[JSON] AdminData
  :<|> RequireRole "cookie-auth" 'Host  :> "dashboard" :> Get '[JSON] Dashboard
  :<|> AuthProtect "cookie-auth"        :> "profile" :> Get '[JSON] Profile
  :<|> "public" :> Get '[JSON] PublicInfo
```

- `/admin` — requires `Admin` role or higher
- `/dashboard` — requires `Host` role or higher
- `/profile` — requires authentication only (any role)
- `/public` — no authentication

## Alternative: Permission Sets

`CheckRole` is not limited to linear hierarchies. Because `checkRole` is just a predicate, you can use set membership or any custom logic:

```haskell
data Permission = CanRead | CanEdit | CanDelete
  deriving (Eq, Ord, Show)

instance CheckRole 'CanEdit where
  type RoleType 'CanEdit = Set Permission
  checkRole _ perms = CanEdit `Set.member` perms

instance CheckRole 'CanDelete where
  type RoleType 'CanDelete = Set Permission
  checkRole _ perms = CanDelete `Set.member` perms
```

This works with the same `RequireRole` combinator — no changes needed.

## Complete Minimal Example

A self-contained example you can adapt:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import App.Auth.Role (HasRole(..), RequireRole, CheckRole(..), Proxy(..))
import Control.Monad.Except (throwError)
import Network.Wai (Request, requestHeaders)
import Network.Wai.Handler.Warp (run)
import Servant.API (Get, JSON, type (:<|>) (..), type (:>))
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server (Context(..), Handler, Server, err401, serveWithContext)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)

-- Role type: Ord encodes the permission hierarchy
data Role = Member | Moderator | Admin
  deriving (Eq, Ord, Show)

-- Auth result type
data Auth = Auth { authUser :: String, authRole :: Role }

-- CheckRole instances (checkRole defines what "sufficient" means)
instance CheckRole 'Member    where type RoleType 'Member    = Role; checkRole _ r = r >= Member
instance CheckRole 'Moderator where type RoleType 'Moderator = Role; checkRole _ r = r >= Moderator
instance CheckRole 'Admin     where type RoleType 'Admin     = Role; checkRole _ r = r >= Admin

-- HasRole instance
instance HasRole Auth Role where
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

-- API: same path, different handlers per role level (most restrictive first)
type API =
       RequireRole "my-auth" 'Admin     :> "panel" :> Get '[JSON] String
  :<|> RequireRole "my-auth" 'Moderator :> "panel" :> Get '[JSON] String
  :<|> RequireRole "my-auth" 'Member    :> "panel" :> Get '[JSON] String

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

- `DataKinds` — promote role constructors to types
- `TypeFamilies` — for `RoleType` and `AuthServerData`
- `TypeOperators` — for `:>`
- `PolyKinds`, `ScopedTypeVariables`, `FlexibleInstances`, `MultiParamTypeClasses`, `UndecidableInstances` — needed by the library internals; your code typically only needs `DataKinds`, `TypeFamilies`, and `TypeOperators`

**Cabal dependencies**:

- `web-server-roles`
- `servant` and `servant-server` (you likely already have these)
