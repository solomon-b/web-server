# servant-auth-roles Tutorial

`servant-auth-roles` gives Servant a `RequireRole` combinator for declarative,
type-level role authorization. Instead of checking permissions inside handlers,
you declare the required role in the API type.

The combinator authenticates, decides whether the caller's role is sufficient
under a relation you choose, and hands the handler a compile-time proof that the
check passed. Role-gated operations can then demand that proof in their own
signatures, so the check cannot be forgotten and never has to run twice.

One Template Haskell splice builds all role machinery.

## The model in a minute

- **Your role is a plain enum.** `data UserRole = Viewer | Editor | Admin`.
- **Auth is indexed by the role** at the type level, as in `RoleAuth (r :: UserRole)`.
  The role lives in the type. The value carries whatever session data you have.
- **Sufficiency is a decidable relation.** Three are built in. A role can be at
  least the required one (hierarchy), exactly it (flat), or the required
  permission can be a member of a held set.
- On a match the handler receives a `Satisfies` value carrying the proof. On a
  mismatch the combinator returns a non-fatal 403, so same-path alternatives are
  still tried. An unauthenticated request is a fatal 401.

## Quick start: a role hierarchy

### 1. The usual scary incantations

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- plus a GHC2021 (or later) default-language, which supplies TypeApplications,
-- ScopedTypeVariables, FlexibleContexts, RankNTypes, etc.

module MyProject where

import Servant.Auth.Roles.TH
import Control.Monad.Except (throwError)
import Network.Wai (Application, Request, requestHeaders)
import Servant.API (Get, JSON, Post, type (:<|>), type (:>))
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server (Context (..), Server, err401, serveWithContext)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
```

### 2. Define the role type and derive the machinery

```haskell
data UserRole = Viewer | Editor | Admin

$(deriveOrdRole ''UserRole)
```

`deriveOrdRole` uses the type's constructor ordering as the hierarchy. In our
example `Admin > Editor > Viewer`. A role is sufficient when `required <=
actual`. The splice generates the internal machinery and one constraint alias
per constructor (`IsAtleastViewer`, `IsAtleastEditor`, `IsAtleastAdmin`).

### 3. Index your auth type by the role

Define `RoleAuth` to carry whatever you have after authenticating (a user
record, a session, nothing at all). The role is the phantom type index `r`, not
a field:

```haskell
newtype RoleAuth (r :: UserRole) = RoleAuth { userName :: String }

type instance AuthServerData (AuthProtect "role-auth") = SomeRole RoleAuth
```

### 4. Write the auth handler

This is ordinary, effectful `AuthHandler` code. Parse a cookie, hit the session
store, whatever your app does. The only library-specific step is the final
`someUserRole`, which reflects the runtime role into the type index:

```haskell
-- This is generated for you:
-- someUserRole :: UserRole -> (forall r. authF r) -> SomeRole authF

roleAuthHandler :: AuthHandler Request (SomeRole RoleAuth)
roleAuthHandler = mkAuthHandler $ \req ->
  case lookup "X-Role" (requestHeaders req) of
    Just "viewer" -> pure (someUserRole Viewer (RoleAuth "Reed"))
    Just "editor" -> pure (someUserRole Editor (RoleAuth "Lyxia"))
    Just "admin"  -> pure (someUserRole Admin  (RoleAuth "Sandy"))
    _             -> throwError err401
```

### 5. Gate routes and write handlers

`RequireRole "role-auth" 'Admin` takes the auth tag (a `Symbol`, the same as
`AuthProtect`) and the required role as a promoted constructor (`'Admin`), so a
typo is a compile error. The handler receives `Satisfies 'Admin RoleAuth`:

```haskell
type AdminAPI = RequireRole "role-auth" 'Admin :> "admin" :> Get '[JSON] String

adminServer :: Server AdminAPI
adminServer (Satisfies _ auth) = pure ("hi " <> userName auth)
```

### 6. Wire it up

The context is the same `AuthHandler` that `AuthProtect` uses, so you can mix
`AuthProtect` and `RequireRole` in one Servant API.

```haskell
app :: Application
app = serveWithContext (Proxy @AdminAPI) (roleAuthHandler :. EmptyContext) adminServer
```

## Spending the proof

In step 5 we used `Satisfies _ auth`, which ignores the proof and takes only the
auth value. Matching the proof constructor instead brings its constraint into
scope, so a role-gated function can demand it and run with no runtime check. This
is an approximation of the Ghosts of Departed Proofs technique.

```haskell
-- IsAtleastAdmin r is generated. It means ('Admin <= r) ~ 'True
banUser :: IsAtleastAdmin r => RoleAuth r -> String
banUser auth = "banned by " <> userName auth

adminServer :: Server AdminAPI
adminServer (Satisfies UserRoleProof auth) = pure (banUser auth)
--                     ^^^^^^^^^^^^^ matching the proof releases the constraint
```

`banUser` can only be called with proof that the role is at least `Admin`. The
proof was established once at the auth boundary. It flows into `banUser` through
the types and is erased at runtime.

## The three relations

Pick the deriver that matches how your roles relate. Each is one splice.

| Deriver                | Sufficient when                             | Proof constructor  | Constraint alias   |
|------------------------|---------------------------------------------|--------------------|--------------------|
| `deriveOrdRole ''T`    | `required <= actual` (hierarchy, via `Ord`) | `TProof`           | `IsAtleast<Con> r` |
| `deriveEqRole ''T`     | `required == actual` (flat, via `Eq`)       | `TProof`           | `Is<Con> r`        |
| `deriveMemberRole ''T` | `required ∈ actual` (permission set)        | `THere` / `TThere` | `Has<Con> ps`      |

### Exact match (`deriveEqRole`)

For flat, unordered roles where "at least" is meaningless, a request must match
exactly:

```haskell
data Region = US | EU | APAC
$(deriveEqRole ''Region)

newtype RegionAuth (r :: Region) = RegionAuth { tenant :: String }
type instance AuthServerData (AuthProtect "region-auth") = SomeRole RegionAuth

type EUAPI = RequireRole "region-auth" 'EU :> "eu" :> Get '[JSON] String

-- Is<Con> aliases mean (r ~ 'Con), so an EU-only operation is enforced:
gdprExport :: IsEU r => RegionAuth r -> String
gdprExport auth = "GDPR export for " <> tenant auth

euServer (Satisfies RegionProof auth) = pure (gdprExport auth)
```

### Permission sets (`deriveMemberRole`)

The held role is a set (`[Permission]`). A route requires a single permission,
sufficient when it is a member. Holding one permission implies nothing about the
others:

```haskell
data Permission = PermRead | PermWrite | PermManage
$(deriveMemberRole ''Permission)

data PermAuth (ps :: [Permission]) = PermAuth   -- often no fields
type instance AuthServerData (AuthProtect "perm-auth") = SomeRole PermAuth

-- the packer takes the whole held set:
--   pure (somePermission [PermRead, PermWrite] PermAuth)

type WriteAPI = RequireRole "perm-auth" 'PermWrite :> "write" :> Post '[JSON] ()
```

A required list can be used to impose multiple required permissions:

```haskell
type ReadWriteAPI =
  RequireRole "perm-auth" '[PermRead, PermWrite] :> "readwrite" :> Post '[JSON] ()
```

`deriveMemberRole` generates a `Has<Con>` alias (`type HasPermWrite ps = Member
'PermWrite ps`), and the library provides releasers that turn a proof into that
constraint. A permission-gated function then demands `Has<Con> ps =>`, and the
handler releases it:

```haskell
writeUser :: HasPermWrite ps => PermAuth ps -> IO ()

-- single-permission gate: release from the element proof
writeH (Satisfies proof auth) = liftIO (withMember proof (writeUser auth))

-- required-list gate: withAllMembers releases every membership at once,
-- so writeUser (needing HasPermWrite) is callable
readWriteH (Satisfies proof auth) = withAllMembers proof (liftIO (writeUser auth))
```

## Same-path alternatives (fallthrough)

Because a role mismatch is a non-fatal 403, you can serve different handlers for
different roles on the same path. Servant tries each `:<|>` alternative until
one's role check passes:

```haskell
type PanelAPI =
       RequireRole "role-auth" 'Admin  :> "panel" :> Get '[JSON] AdminView
  :<|> RequireRole "role-auth" 'Editor :> "panel" :> Get '[JSON] EditorView
  :<|> RequireRole "role-auth" 'Viewer :> "panel" :> Get '[JSON] ViewerView
```

Three things to know:

1. **Ordering (Ord scheme only).** Order routes most-restrictive-first. With the
  `>=` relation an `Admin` satisfies the `Viewer` gate too, so a `Viewer` route
  placed first would shadow the more restrictive ones. The exact and membership
  schemes are order-independent (each role matches exactly one gate).
2. **Re-authentication.** Each alternative runs the auth handler independently.
  A `Viewer` reaching the third alternative above runs auth three times. Keep it
  in mind if your handler is expensive.
3. **401 is fatal.** An unauthenticated request short-circuits at the first
  `RequireRole` with a 401 and never falls through. A public route sitting
  behind role gates on the same path is therefore unreachable by anonymous
  callers. It is dead code, not an anonymous fallback.

## Mixing with AuthProtect

`RequireRole` and `AuthProtect` share the same `AuthHandler` and tag, so you can
mix them freely:

```haskell
type API =
       RequireRole "role-auth" 'Admin :> "admin"   :> Get '[JSON] AdminData
  :<|> AuthProtect  "role-auth"        :> "profile" :> Get '[JSON] Profile
  :<|> "public" :> Get '[JSON] PublicInfo
```

Under a tag whose `AuthServerData` is `SomeRole RoleAuth`, a plain
`AuthProtect "role-auth"` handler receives the existential `SomeRole RoleAuth`.
Unpack it with `\(SomeRole _ auth) -> ...`.

## What a deriver generates

`$(deriveOrdRole ''UserRole)` emits, for the role type:

- the singletons (`Sing`, `SingKind`, `SOrd`/`SEq`/decidable equality), hidden
- the `Decidable` instance and its `ActualK`, the decision procedure
- the `Proof` data instance and its constructor (`UserRoleProof`)
- the packer `someUserRole` (singleton-free signature)
- one constraint alias per constructor (`IsAtleastViewer`, `IsAtleastEditor`,
  `IsAtleastAdmin`)

`deriveEqRole` is the same with `TProof` and `Is<Con>` aliases. `deriveMemberRole`
emits `THere`/`TThere` element witnesses, a subset relation for `[Role]`
requirements (`TAllNil`/`TAllCons`), and `Has<Con>` aliases. Spend a membership
proof as a constraint with `withMember` (single) or `withAllMembers` (list).


# Shout outs

Special thanks to @IsoVector.
