# Changelog

All notable changes to `web-server-otel` are documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/) (PVP).

## Unreleased

### Changed (breaking)

- Removed `OtelServerContext` and the fixed `runServerWithOtel`.
  `runServerWithOtelContext` is now the sole server entry point: the app supplies
  its own Servant auth `Context` (its own handler and principal), and the OTel
  tracer is prepended automatically. Follows `web-server-core` making the auth
  principal downstream-defined.
- `toStatementAttributes` now gives `<redacted>` for each parameter value,
  and it removes the literals from the `sql` attribute with `redactSql`.

### Security

- Logs and trace spans no longer hold secrets. With the `StdOut` exporter,
  spans go to the same output as the logs.
  - The "db query" log and the `sql` span attribute use `redactSql`.
  - Database error logs and span events use `renderUsageError` and
    `renderCommandError` in place of `show`. The `params` attribute of a
    `logical_query_error` event gives `<redacted>` for each value.
  - The `result` attribute of the `execution_result` and `handler result`
    events is now `<redacted>`. A query result or a response can hold a token
    or a session id.
  - `WithSpan` and `wrapInSpan` remove secrets from the request with
    `redactRequest` before they show it. The request held all headers, with
    the session cookie.
  - `wrapInSpan` and `handlerSpan` remove secret headers from a Servant error,
    for example `Set-Cookie`, before they show it.

## 0.1.0.0 — 2026-01-30

Initial release, factored out of `web-server-core`. Provides
OpenTelemetry tracing wiring for the framework: an `Effects.Observability`
module, a `WithSpan` Servant combinator (with `HasLink`/`HasServer`
instances), Jaeger / stdout exporters, and helpers for attaching the
OTEL tracer to the Servant `Context`. See `git log` for pre-baseline
history.
