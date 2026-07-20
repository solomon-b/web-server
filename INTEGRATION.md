# Adding servant-auth-roles to an existing Servant app

This is a migration guide for an app that already authenticates with Servant's
`AuthProtect` — you have an `AuthHandler Request Auth` in your context and routes
written as `AuthProtect "tag" :> …`. It shows the concrete changes to add
role gates. For the concepts and the three role schemes, see
[`TUTORIAL.md`](./TUTORIAL.md).

`servant-auth-roles` is standalone: it imposes no shape on your auth data. `Auth`
below is *your* type; the only requirement is that it becomes indexed by the
role.

## The five changes

Assume you start with:

```haskell
data Auth = Auth { authUser :: User, authSession :: Session }
type instance AuthServerData (AuthProtect "cookie-auth") = Auth

authHandler :: AuthHandler Request Auth
authHandler = mkAuthHandler $ \req -> do
  ...effectful: cookie, session, user lookup...
  pure (Auth user session)
```

### 1. Set the role type up

Define your role type (or reuse an existing one) and run the deriver that matches
how the roles relate — `deriveOrdRole` for a hierarchy, `deriveEqRole` for flat
exact match, `deriveMemberRole` for permission sets:

```haskell
import Servant.Auth.Roles.TH

data UserRole = Member | Host | Admin
$(deriveOrdRole ''UserRole)
```

### 2. Index the auth type by the role

Add the role as a **phantom type parameter** and point `AuthServerData` at
`SomeRole`:

```haskell
data Auth (r :: UserRole) = Auth { authUser :: User, authSession :: Session }

type instance AuthServerData (AuthProtect "cookie-auth") = SomeRole Auth
```

The role is in the *type index* `r`, not a field. The record fields are
unchanged — carry the same session data you always did.

### 3. Pack the role in the auth handler

Your handler already produces an `Auth`; now it produces `SomeRole Auth` by
finishing with the generated `some<Role>` packer, which reflects the runtime role
into the index. Everything before it — the effectful part — is untouched:

```haskell
authHandler :: AuthHandler Request (SomeRole Auth)
authHandler = mkAuthHandler $ \req -> do
  ...effectful: cookie, session, user lookup...       -- unchanged
  let role = ...                                       -- however your app gets the role
  pure (someUserRole role (Auth user session))        -- was: pure (Auth user session)
```

### 4. Gate routes

Swap `AuthProtect "tag"` for `RequireRole "tag" 'Role` on the routes you want
gated. The required role is a promoted constructor (`'Host`), checked at compile
time:

```haskell
-- before
type DashboardAPI = AuthProtect "cookie-auth" :> "dashboard" :> Get '[HTML] View

-- after
type DashboardAPI = RequireRole "cookie-auth" 'Host :> "dashboard" :> Get '[HTML] View
```

### 5. Update the gated handlers

A gated handler's auth argument changes from `Auth` to `Satisfies 'Role Auth`.
Pattern-match to recover the auth value (and, if you want it, the proof):

```haskell
-- before:  dashboard :: Auth -> Handler View
-- after:
dashboard :: Satisfies 'Host Auth -> Handler View
dashboard (Satisfies _ auth) = render (authUser auth)
```

To let a role-gated helper demand the check as a constraint in its own type,
release it from the proof: match the proof constructor (Ord/Eq schemes) or use
`withMember` / `withAllMembers` (member scheme). See "Spending the proof" in the
tutorial.

## Auth-only routes on the same tag

Once `AuthServerData (AuthProtect "cookie-auth") = SomeRole Auth`, **every** route
under that tag maps to `SomeRole Auth` — including plain `AuthProtect "cookie-auth"`
routes you did not gate. Their handlers now receive the existential and unpack it:

```haskell
profile :: SomeRole Auth -> Handler View
profile (SomeRole _ auth) = render (authUser auth)
```

If you would rather not touch existing `AuthProtect` handlers, give the
role-indexed auth its **own tag** (a second `AuthServerData` instance and context
entry) and leave the original tag as-is.

## Operational notes

- **Route ordering (Ord scheme).** Same-path alternatives must be ordered
  most-restrictive-first; a higher role satisfies a lower gate, so a permissive
  route placed first shadows the stricter ones. Exact and membership schemes are
  order-independent.
- **Re-authentication.** Each `:<|>` alternative runs the auth handler
  independently; a request that falls through N alternatives authenticates N+1
  times. If your handler is expensive, prefer distinct paths over same-path
  fallthrough.
- **401 is fatal, 403 is not.** An unauthenticated request stops at the first
  `RequireRole` (401) and never falls through, so a public route behind role
  gates on the same path is unreachable by anonymous callers.
- **Forgot a deriver?** Using `RequireRole` on a role type with no deriver is a
  compile error that names the type and points at the derivers, not a raw
  missing-instance error.
