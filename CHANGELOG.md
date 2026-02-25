# Changelog

## Unreleased — OWASP Session Management Hardening

### Security

- **Session ID generation**: Replaced `uuid_generate_v1mc()` (v1, timestamp-based) with `gen_random_uuid()` (v4, CSPRNG-backed, 122 bits of entropy) for session ID generation. Removed the `uuid-ossp` extension dependency (PostgreSQL 13+ provides `gen_random_uuid()` natively).

- **Idle timeout**: Added `last_activity_at` column to `server_sessions` and idle timeout enforcement in `authHandler`. Sessions now expire after 30 minutes of inactivity (configurable via `SessionConfig`). Activity updates are throttled to one write per 5 minutes to limit DB load.

- **Cookie clearing on logout**: Added `mkCookieSessionExpired` helper that builds a `Set-Cookie` header with `Max-Age=0` to clear the session cookie from the browser on logout. Previously, logout only expired the server-side session row.

- **Cache-Control headers**: Added `noCacheAuthenticated` middleware in `mkApp` that sets `Cache-Control: no-store` on responses to authenticated requests (those carrying a session cookie). Public/unauthenticated responses are unaffected.

- **Session activity logging**: Added structured logging for session lifecycle events — creation (with user ID, IP, user-agent), logout, and idle timeout expiry. Session IDs are truncated to 8 characters in logs to avoid leaking full tokens.

### Added

- `SessionConfig` type with `sessionIdleTimeout` and `sessionActivityThrottle` fields.
- `defaultSessionConfig` — 30 minute idle timeout, 5 minute activity throttle.
- `authHandlerWith` — accepts a `SessionConfig` for custom timeout configuration. The existing `authHandler` delegates to it with `defaultSessionConfig`.
- `mkCookieSessionExpired` — builds a cookie-clearing `Set-Cookie` header value for use in logout responses.
- `noCacheAuthenticated` — WAI middleware that adds `Cache-Control: no-store` to authenticated responses.
- `truncateSessionId` — now exported from `App.Auth` for use in logging and testing.
- `ServerSessions.touchSession` — updates `last_activity_at` for a session.
- `last_activity_at` field on `ServerSessions.Model` and `ServerSessions.Domain`.

### Changed

- **Example logout handler** now returns a `Set-Cookie` header (via `mkCookieSessionExpired`) alongside the redirect, clearing the browser cookie on logout. The handler signature gained an `Environment` parameter.
- `login` and `expireServerSession` in `App.Auth` now emit structured log messages on success (session created / session logout).

### Tests

- `App.AuthSpec` (new) — pure tests for `mkCookieSession` (required attributes, Domain presence/absence), `mkCookieSessionExpired` (Max-Age=0, cookie clearing), and `truncateSessionId` (8-char output).
- `ServerSessionsSpec` — added DB integration tests for `touchSession` (timestamp advances), `expireSession` (session becomes invisible), and `last_activity_at` round-trip.

### Migration (for existing downstream consumers)

#### Database

```sql
-- 1. Switch session ID default from uuid_generate_v1mc() to gen_random_uuid()
ALTER TABLE server_sessions ALTER COLUMN id SET DEFAULT gen_random_uuid();
DROP EXTENSION IF EXISTS "uuid-ossp";

-- 2. Add idle timeout tracking
ALTER TABLE server_sessions
  ADD COLUMN last_activity_at TIMESTAMPTZ NOT NULL DEFAULT now();

-- 3. Drop single-session trigger (replaced by CTE in insertServerSession
-- that caps active sessions at 5 per user)
DROP TRIGGER IF EXISTS check_only_one_active_session ON server_sessions;
DROP FUNCTION IF EXISTS check_only_one_active_session();
```

#### Code

- **Logout handlers** should send a `Set-Cookie` clearing header alongside the server-side session expiry. Use `mkCookieSessionExpired env mDomain` to build the header value. Without this, the expired session cookie lingers in the browser until it is naturally discarded.
- **`ServerSessions.Model` / `Domain`** gained `mLastActivityAt` / `dLastActivityAt`. Exhaustive pattern matches without `RecordWildCards` will need updating.
