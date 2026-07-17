{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- singletons emits defunctionalization symbols (e.g. ViewerSym0) we don't use.
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Example-local, role-indexed authorization built on top of
-- @servant-auth-roles@ and core's 'App.Auth.authHandlerFor'.
--
-- The 'Role' hierarchy (@Viewer < Editor < Admin@) is turned into the
-- singletons, proof, and packer machinery by a single 'deriveOrdRole' splice.
-- 'roleAuthHandler' reuses core's cookie-session validation (so idle-timeout
-- and activity-throttle semantics are shared) and additionally reflects the
-- caller's stored role into the type index, packing it as 'SomeRole'.
module Auth.Roles
  ( Role (..),
    RoleAuth (..),
    roleAuthHandler,
    getUserRole,
    roleFromText,
  )
where

--------------------------------------------------------------------------------

import App.Auth (CookieAuth, authHandlerFor, defaultSessionConfig)
import App.Config (Environment)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Effects.Database.Tables.ServerSessions qualified as ServerSessions
import Effects.Database.Tables.User qualified as User
import Hasql.Interpolate (OneColumn (getOneColumn), OneRow (getOneRow), interp, sql)
import Hasql.Pool qualified as HSQL
import Hasql.Session qualified as HSQL
import Hasql.Statement (Statement)
import Network.Wai (Request)
import Servant.Auth.Roles.TH
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData)

--------------------------------------------------------------------------------

-- | Hierarchical application roles. 'deriveOrdRole' derives the ordering
-- @Viewer < Editor < Admin@, so a gate on @'Editor@ is satisfied by @Editor@
-- and @Admin@ but not @Viewer@.
data Role = Viewer | Editor | Admin
  deriving (Show)

$(deriveOrdRole ''Role)

-- | The app's auth principal: the caller's role as a value (for display), plus
-- their validated user and session. It is also indexed by the 'Role' as a
-- phantom so 'Servant.Auth.Roles.RequireRole' can gate on it. (The library's
-- proof carries no role value, so a handler that wants to show the role reads it
-- from here.) Core defines no principal of its own, so the app owns this one.
data RoleAuth (r :: Role) = RoleAuth {raRole :: Role, raUser :: User.Model, raSession :: ServerSessions.Model}

-- | Point core's 'CookieAuth' scheme at this app's role-indexed principal. Every
-- route on the tag is then served by one handler: plain 'CookieAuth' routes
-- receive the existential 'SomeRole', @RequireRole@ routes receive a 'Satisfies'
-- proof.
type instance AuthServerData CookieAuth = SomeRole RoleAuth

--------------------------------------------------------------------------------

-- | Read a user's stored role text from the @users.role@ column. The user is
-- guaranteed to exist here (the session was already validated against the
-- @users@ table), so a single-row decode is safe.
getUserRole :: User.Id -> Statement () Text
getUserRole userId =
  getOneColumn . getOneRow
    <$> interp
      False
      [sql|
    SELECT role
    FROM users
    WHERE id = #{userId}
  |]

-- | Parse the stored role text, defaulting anything unrecognized to the least
-- privileged role.
roleFromText :: Text -> Role
roleFromText = \case
  "Editor" -> Editor
  "Admin" -> Admin
  _ -> Viewer

-- | A role-indexed cookie-session auth handler. It runs core's
-- 'authHandlerFor' — sharing the exact session validation, idle-timeout, and
-- activity-throttle logic — then reads the caller's role and packs the
-- validated user and session as a 'SomeRole'. A database error reading the role
-- fails closed to 'Viewer' (the least privileged role).
roleAuthHandler :: HSQL.Pool -> Environment -> AuthHandler Request (SomeRole RoleAuth)
roleAuthHandler pool env =
  authHandlerFor
    ( \user session -> do
        result <- liftIO $ HSQL.use pool (HSQL.statement () (getUserRole (User.mId user)))
        let role = either (const Viewer) roleFromText result
        pure (someRole role (RoleAuth role user session))
    )
    pool
    env
    defaultSessionConfig
