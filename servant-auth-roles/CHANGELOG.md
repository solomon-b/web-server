# Changelog

All notable changes to `servant-auth-roles` are documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/) (PVP).

## Unreleased

- `deriveMemberRole` now also generates a subset relation, so one gate can
  require a list of permissions (`RequireRole "tag" '[CanRead, CanWrite]`),
  satisfied when all are held. Adds `<T>AllNil`/`<T>AllCons` witnesses.
- Membership is now spendable as a constraint. New exports `Member`,
  `AllMembers`, `withMember`, `withAllMembers`; `deriveMemberRole` also generates
  `Has<Con>` aliases (`type HasCanWrite ps = Member 'CanWrite ps`).

## 0.1.0.0 — 2026-07-16

Initial release. Provides `RequireRole`, a Servant combinator for declarative,
type-level role authorization. It authenticates, decides whether the caller's
role satisfies the requirement under a relation you choose, and hands the
handler a compile-time proof of the check.

Three relations are built in, each set up with one Template Haskell splice on a
plain enum: `deriveOrdRole` (hierarchy, via `Ord`), `deriveEqRole` (exact match,
via `Eq`), and `deriveMemberRole` (permission-set membership). Each deriver
generates the singletons, the `Decidable` instance, the `Proof` witness, a
singleton-free packer, and per-constructor constraint aliases. See
`TUTORIAL.md`.
