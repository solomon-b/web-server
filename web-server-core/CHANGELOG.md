# Changelog

All notable changes to `web-server-core` are documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/) (PVP).

## Unreleased

### Changed (breaking)

- `Domain.Types.EmailAddress.EmailAddress` now wraps
  `Text.Email.Validate.EmailAddress` instead of `CI Text`. Construction
  validates RFC syntax and lowercases the address; all `FromJSON`,
  `FromHttpApiData`, and Hasql `DecodeValue` instances now reject
  syntactically invalid addresses.
- `mkEmailAddress :: Text -> Either ValidationFailure EmailAddress`
  (previously total: `Text -> EmailAddress`).
- Removed `isValid` and `validate` — every `EmailAddress` is valid by
  construction; use `mkEmailAddress` instead.

## 0.2.0.0 — 2026-05-04

### Changed (breaking)

- `App.Config.PostgresConfig` is now a newtype around a single libpq
  connection string read from `APP_POSTGRES_CONNECTION_STRING`. The five
  `APP_POSTGRES_{HOST,PORT,DB,USER,PASSWORD}` env vars and the
  corresponding fields are gone.
- `App.Auth.login` now returns `LoginResult` (carrying session id, expiry,
  and the chosen duration) instead of bare `ServerSessions.Id`.

### Added

- `App.Auth.loginWithExpiry :: NominalDiffTime -> ...` for caller-supplied
  session lifetimes.
- `App.Auth.LoginResult` record pairing the session id with its duration
  and computed expiry, so cookie helpers can avoid drift.
- `App.Auth.mkCookieSessionWithMaxAge` appends `Max-Age=<seconds>` so the
  browser-side cookie and DB-side session expire together.

## 0.1.0.0 — 2026-01-30

Baseline release as factored out of the example server. Provides the core
framework on top of Servant + Hasql: HKD-based environment configuration,
cookie-session authentication with per-environment cookie names and
multi-session support, a `MonadDB`-style database effects layer with
hasql pool and transaction helpers, an OpenTelemetry-aware application
monad and Servant context, structured request/error logging, base
domain types, base table modules (User, ServerSessions, BlogPosts,
Products, Images, MailingList), Lucid HTML helpers, and a test suite
covering auth, optics, and database integration via `tmp-postgres`.
See `git log` for the full pre-baseline history.
