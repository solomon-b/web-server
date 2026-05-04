# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Haskell web server project that provides a full-stack web application with user authentication, blog functionality, and an e-commerce store. It uses Servant for the web framework, Hasql for database interactions, and Lucid for HTML templating.

## Versioning and Changelogs

Every package (`web-server-core`, `web-server-otel`, `xmlhtml-lens`,
`xmlhtml-qq`, `example`) has a `CHANGELOG.md` and follows the
[Haskell PVP](https://pvp.haskell.org/) (`A.B.C.D`).

**For every change to a package, you MUST add a bullet under that
package's `## Unreleased` section in its `CHANGELOG.md`.** This applies
to features, fixes, refactors, and breaking changes — anything a
downstream consumer might want to know about. Only purely internal
changes that have no observable effect (e.g. comment-only edits,
moving a file with no API change) can skip the changelog.

When the change is breaking (PVP-major: removed/renamed exports,
changed type signatures, changed runtime behavior of a documented
API), call it out under a `### Changed (breaking)` heading and bump
the cabal `version` field's first two components on the next release.

If you are unsure whether a change warrants a changelog entry, add
one — entries are cheap, missing context is expensive.

## Build System and Commands

This project uses **Cabal** as the primary build system with **Just** as a task runner:

### Essential Commands
- `just run` - Run the backend service (starts server on port 2000)
- `just build` - Build all Haskell packages with tests and benchmarks. USE THIS ONE TO BUILD THE PROJECT UNLESS YOU REALLY NEED TO USE CABAL DIRECTLY.
- `just test` - Run all test suites with detailed output
- `just clean` - Delete all build artifacts
- `just haddock` - Build documentation

### Code Quality
- `just format` - Auto-format all Haskell and Nix source code
- `just format-changed` - Format only changed files
- `just check-format` - Check formatting without modifying files
- `just lint-shell` - Lint shell scripts with shellcheck

### Database Operations
- `just postgres-dev-start` - Start development PostgreSQL container
- `just postgres-dev-stop` - Stop development PostgreSQL container
- `just migrations-run` - Run SQL migrations
- `just migrations-add MIGRATION` - Create a new migration file
- `just migrations-reset` - Reset the database (destructive)

### Observability
- `just jaeger-start` - Start Jaeger tracing service
- `just jaeger-stop` - Stop Jaeger tracing service

### Testing Utilities
- `just get-users` - Curl localhost:2000/user for quick API testing
- `just get-user id` - Get a specific user by ID

### Running Single Tests
To run a single test module or spec:
```bash
cabal test all --test-show-details=direct --test-option=--match --test-option="pattern"
```
For example:
```bash
cabal test all --test-show-details=direct --test-option=--match --test-option="UserSpec"
```

## Architecture

### Module Structure

The project is organized into several major components:

1. **API Layer** (`API/`) - Servant API definitions organized by feature:
   - User management (registration, login, logout, password reset)
   - Blog posts (CRUD operations, publishing)
   - Store/Products (e-commerce functionality)
   - Admin interface
   - Static file serving

2. **Application Layer** (`App/`) - Core application configuration and context:
   - `App.hs` - Main application setup with tracing and database pool
   - `App.Config` - Configuration management
   - `App.Context` - Application context and dependency injection
   - `App.Monad` - Custom monad stack for the application
   - `App.Auth` - Authentication middleware

3. **Domain Layer** (`Domain/Types/`) - Core business types:
   - Email addresses, display names, product names
   - Type-safe wrappers around primitive types

4. **Effects Layer** (`Effects/`) - Effect definitions and implementations:
   - `Effects.Database` - Database operations with Hasql
   - `Effects.MailSender` - Email sending capabilities
   - `Effects.Clock` - Time operations
   - `Effects.Observability` - OpenTelemetry integration

5. **Database Tables** (`Effects/Database/Tables/`) - Typed database operations:
   - User management
   - Blog posts
   - Products
   - Images
   - Server sessions
   - Mailing list

6. **Components** (`Component/`) - Reusable UI components:
   - Forms (login, register, blog post, product)
   - Navigation bar
   - Page frame/layout

### Tech Stack

- **Web Framework**: Servant (type-safe REST API)
- **Database**: PostgreSQL with Hasql (type-safe SQL)
- **HTML Templating**: Lucid2 (type-safe HTML DSL)
- **Authentication**: Custom JWT-based auth with secure sessions
- **Observability**: OpenTelemetry with Jaeger for distributed tracing
- **Database Migrations**: SQLx for migration management
- **Deployment**: Nix flakes for reproducible builds

### Key Features

- **User Management**: Registration, login, logout, password reset
- **Blog System**: Create, edit, delete, publish/unpublish blog posts
- **E-commerce Store**: Product management with admin interface
- **Admin Dashboard**: Administrative interface for content management
- **Image Upload**: File upload handling with secure storage
- **Email Integration**: SMTP email sending for notifications
- **Security**: Session-based authentication with admin role checking

### Development Workflow

1. Start development database: `just postgres-dev-start`
2. Run migrations: `just migrations-run`
3. Start the server: `just run`
4. Optionally start tracing: `just jaeger-start`

The server runs on `localhost:2000` by default. The application uses a multi-package Cabal project with four main packages:
- `server` - Example web application demonstrating the framework
- `web-server-core` - Core web server framework and shared functionality
- `xmlhtml-lens` - Utilities for XML/HTML manipulation with lens
- `xmlhtml-qq` - QuasiQuoter for XML/HTML templating

**Note**: The `server/` package serves as an example implementation. The main project is `web-server-core` along with the supporting utility packages.

### Environment Setup

The project uses Nix for dependency management. The development shell provides all necessary tools including GHC, Cabal, PostgreSQL client, and formatting tools. Essential environment variables include `DATABASE_URL` for database connections.

### Testing

The test suite uses HSpec with Hedgehog for property-based testing. Tests are organized by module structure and include both unit tests and database integration tests using tmp-postgres for isolated test databases.
