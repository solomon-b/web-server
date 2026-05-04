# Changelog

All notable changes to the `example` server are documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/) (PVP).

## Unreleased

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
