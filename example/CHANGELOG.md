# Changelog

All notable changes to the `example` server are documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/) (PVP).

## Unreleased

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
