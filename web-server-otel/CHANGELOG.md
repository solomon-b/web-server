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

## 0.1.0.0 — 2026-01-30

Initial release, factored out of `web-server-core`. Provides
OpenTelemetry tracing wiring for the framework: an `Effects.Observability`
module, a `WithSpan` Servant combinator (with `HasLink`/`HasServer`
instances), Jaeger / stdout exporters, and helpers for attaching the
OTEL tracer to the Servant `Context`. See `git log` for pre-baseline
history.
