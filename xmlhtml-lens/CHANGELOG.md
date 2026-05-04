# Changelog

All notable changes to `xmlhtml-lens` are documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/) (PVP).

## Unreleased

## 0.1.0.0 — 2024-12-09

Initial release, factored out of the example server. Provides optics
(prisms, traversals, and folds) for navigating and editing
`xmlhtml` `Node`/`Document` trees, including helpers like `_navbar`
and `FocusedElement`-aware combinators, with an accompanying test
suite. See `git log` for pre-baseline history.
