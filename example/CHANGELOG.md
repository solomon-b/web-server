# Changelog

All notable changes to the `example` server are documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/) (PVP).

## Unreleased

### Added

- Role-based authorization demo. A `Role` hierarchy (`Viewer < Editor < Admin`)
  with a `users.role` column (new migration) and a `/admin` route gated on
  `Editor` via `servant-auth-roles`' `RequireRole`. One role-indexed auth handler
  (`Auth.Roles.roleAuthHandler`, built on core's `authHandlerFor`) backs both the
  gated route and the plain cookie-auth Dashboard routes.

### Fixed

- `POST /user/logout` is no longer behind the cookie-auth handler. Because a
  session past its idle timeout fails authentication, logout previously
  returned 401 for exactly the sessions that most needed clearing, leaving a
  dead cookie in the browser with no way to shed it short of clearing cookies
  by hand. The route now reads the `Cookie` header directly, expires the named
  server session on a best-effort basis, and always responds with the clearing
  `Set-Cookie`, making logout idempotent and unable to fail.

### Changed (breaking)

- `API.User.Logout.Post.Route` no longer carries the auth combinator and
  `handler`'s second argument is now `Maybe Text` (the raw `Cookie` header)
  rather than the auth principal.

### Changed

- Adopted `web-server-core`'s downstream-owned auth principal: the app defines its
  own `RoleAuth` principal and the `AuthServerData CookieAuth` instance, and serves
  via `runServerWithOtelContext` with its own auth `Context`. The Dashboard routes
  now unpack `SomeRole RoleAuth` instead of taking the removed `Authz`.
- Login and registration flows now use `Auth.loginWithExpiry`
  (via `Auth.login`) and `Auth.mkCookieSessionWithMaxAge`, so the
  session cookie's `Max-Age` matches the DB-side session expiry.

## 0.1.0.0 — 2025-11-29

Baseline release. Demo full-stack web app built on top of
`web-server-core`: registration / login / logout / password reset,
blog CRUD (drafts, publish toggle, markdown editor with image
upload), simple e-commerce store, admin dashboard, mailing list
signup, and Lucid-based templates. See `git log` for the pre-baseline
history (the package was renamed from `server` to `example` at this
point).
