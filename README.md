# Web Server Core

I often find myself writing the same code when starting a fresh Servant project. `web-server-core` encapsulates that work into a small wrapper with built-in support for PostgreSQL, authentication, OpenTelemetry tracing, and HTML templating.

This is a work in progress but is useable as is. The main work to be done is figuring out how to nicely decompose the feature set into a few smaller libs.

## Overview

This repository contains a multi-package Haskell project:

- **web-server-core** - Core utilities and integrations for Servant applications
- **xmlhtml-lens** - Utilities for XML/HTML manipulation with lens
- **xmlhtml-qq** - QuasiQuoter for XML/HTML templating
- **server** - Example application demonstrating the utilities. *deprecated*

## Built-in Support

- **Database**: PostgreSQL integration with Hasql
- **Authentication**: JWT-based session management
- **HTML**: Lucid2 templating with utilities
- **Observability**: OpenTelemetry tracing
- **Email**: SMTP integration for notifications

## Example

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Hello.Get where

import App.Auth (Authz(..))
import App.Monad (App)
import Effects.Observability (handlerSpan)
import Lucid2
import Servant
import Servant.Server.Experimental.Auth (AuthProtect)

type API = AuthProtect "cookie-auth"
        :> "hello"
        :> Capture "name" Text
        :> Get '[HTML] (Html ())

handler :: Authz -> Text -> App (Html ())
handler authz name =
  handlerSpan "helloHandler" name id $ do
    pure $ do
      h1_ "Hello World"
      p_ $ "Welcome, " <> toHtml name <> "!"
      p_ $ "Logged in as: " <> toHtml (userDisplayName $ authzUser authz)
```
