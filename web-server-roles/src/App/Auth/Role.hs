-- | Type-level role authorization for Servant. Auth data is indexed by the
-- user's role. The 'RequireRole' combinator authenticates, decides whether the
-- actual role satisfies the requirement under a relation you define, and hands
-- the handler a 'Proof' of it. The relation is arbitrary, so hierarchies,
-- permission sets, and exact match all work.
--
-- This module is the low-level interface. Most users want "App.Auth.Role.TH",
-- whose derivers generate the 'Decidable' instance, 'ActualK', 'Proof', and
-- singletons for a role type. Working against this module directly, a downstream
-- app provides:
--
-- 1. A role type with singletons (via @singletons-th@'s @genSingletons@).
-- 2. An auth type indexed by the role, and the existential mapping:
--
-- @
-- data Authz (r :: UserRole) = Authz { ... }
-- type instance AuthServerData (AuthProtect "cookie-auth") = SomeRole Authz
-- @
--
-- 3. A 'Decidable' instance saying what "sufficient" means.
-- 4. An 'AuthHandler' that reflects the runtime role into the index with
--    @toSing@ and packs it as 'SomeRole'.
module App.Auth.Role
  ( -- * Combinator
    RequireRole,

    -- * The decidable relation
    Decidable (..),
    ActualK,
    Proof,

    -- * Auth wrappers
    SomeRole (..),
    Satisfies (..),
  )
where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (liftIO)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Singletons (Sing, SingI (..))
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)
import Network.Wai (Request)
import Servant.API (type (:>))
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server
  ( Handler,
    HasContextEntry (..),
    HasServer (..),
    ServerError (..),
    err403,
  )
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, unAuthHandler)
import Servant.Server.Internal.Delayed (addAuthCheck)
import Servant.Server.Internal.DelayedIO (DelayedIO, delayedFail, delayedFailFatal, withRequest)
import Servant.Server.Internal.Handler (runHandler)

--------------------------------------------------------------------------------
-- The decidable relation

-- | Evidence that actual role @r@ satisfies requirement @req@. Each scheme
-- provides a @data instance@ for its role kinds. The value reaches the handler
-- inside 'Satisfies'.
data family Proof (req :: kr) (r :: ka)

-- | The kind of the actual role that @req@ is checked against. Often the same
-- kind as @req@ (a hierarchy), but not always. A required @'Permission@ is
-- checked against an actual @[Permission]@. It is a standalone family rather
-- than an associated type so the fallback 'Decidable' instance need not define it.
type family ActualK (req :: kr) :: Type

-- | The relation deciding whether actual role @r@ (of kind @ActualK req@)
-- satisfies requirement @req@ (of kind @kr@). Required and actual kinds may
-- differ. A required @'Permission@, for instance, is checked against an actual
-- @[Permission]@, which is how non-hierarchical schemes fit.
--
-- You do not normally write this. 'App.Auth.Role.TH.deriveOrdRole',
-- @deriveEqRole@, and @deriveMemberRole@ generate the instance, its 'ActualK',
-- and the 'Proof' witness. Spelled out, a hierarchical relation is:
--
-- @
-- type instance ActualK (req :: UserRole) = UserRole
--
-- data instance Proof (req :: UserRole) (r :: UserRole) where
--   UserRoleProof :: (req <= r) ~ 'True => Proof req r
--
-- instance Decidable (req :: UserRole) where
--   decideRole a b = case a %<= b of STrue -> Just UserRoleProof; SFalse -> Nothing
-- @
class Decidable (req :: kr) where
  -- | Decide @'Proof' req r@ at runtime from singletons of both roles.
  decideRole :: Sing req -> Sing (r :: ActualK req) -> Maybe (Proof req r)

-- | Fallback that gives a useful error when 'RequireRole' is used on a role type
-- with no deriver, instead of a bare missing-instance message. A real, more
-- specific instance always wins. This fires only when none exists.
instance
  {-# OVERLAPPABLE #-}
  ( TypeError
      ( 'Text "No Decidable instance for '"
          ':<>: 'ShowType kr
          ':<>: 'Text "'."
          ':$$: 'Text "Derive one on the role type with Template Haskell, e.g. $(deriveOrdRole ''"
          ':<>: 'ShowType kr
          ':<>: 'Text ")."
          ':$$: 'Text "(deriveOrdRole = hierarchical, deriveEqRole = exact match, deriveMemberRole = permission sets)"
      )
  ) =>
  Decidable (req :: kr)
  where
  -- Unreachable at compile time (the TypeError above fires first). The message
  -- matches that TypeError because this is what surfaces under
  -- @-fdefer-type-errors@, as in the test suite's fixture.
  decideRole _ _ =
    error
      "No Decidable instance for this role type. Run deriveOrdRole, deriveEqRole, \
      \or deriveMemberRole on it. (Normally a compile-time TypeError; seen at \
      \runtime only under -fdefer-type-errors.)"

--------------------------------------------------------------------------------
-- Auth wrappers

-- | The role is known only at runtime, so it is packed existentially with its
-- singleton. @'AuthServerData' ('AuthProtect' tag)@ maps to this.
data SomeRole (authF :: ka -> Type) where
  SomeRole :: Sing (r :: ka) -> authF r -> SomeRole authF

-- | Recover the role-indexed auth constructor from the existential type. The
-- actual-role kind @ka@ is passed explicitly so the result kind is determined.
type family AuthFOf (ka :: Type) (e :: Type) :: ka -> Type where
  AuthFOf ka (SomeRole (f :: ka -> Type)) = f

-- | What a 'RequireRole' handler receives. It holds the role-indexed auth value
-- and a 'Proof' that its role satisfies requirement @req@.
data Satisfies (req :: kr) (authF :: ka -> Type) where
  Satisfies :: Proof req r -> authF r -> Satisfies req authF

--------------------------------------------------------------------------------
-- Combinator

-- | Authenticate and require that the user's role satisfies @req@ under the
-- 'Decidable' relation. On success the handler receives a 'Satisfies' carrying
-- the proof. On failure it is a non-fatal 403, so @(:<|>)@ alternatives are
-- still tried.
data RequireRole (tag :: Symbol) (req :: kr)

instance
  forall kr (tag :: Symbol) (req :: kr) (authF :: ActualK req -> Type) api context.
  ( HasServer api context,
    AuthServerData (AuthProtect tag) ~ SomeRole authF,
    Decidable req,
    SingI req,
    HasContextEntry context (AuthHandler Request (SomeRole authF))
  ) =>
  HasServer (RequireRole tag req :> api) context
  where
  type
    ServerT (RequireRole tag req :> api) m =
      Satisfies req (AuthFOf (ActualK req) (AuthServerData (AuthProtect tag))) -> ServerT api m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy @api) pc nt . s

  route _ context subserver =
    route (Proxy @api) context (subserver `addAuthCheck` withRequest authCheck)
    where
      authHandler' :: Request -> Handler (SomeRole authF)
      authHandler' = unAuthHandler (getContextEntry context)

      authCheck :: Request -> DelayedIO (Satisfies req authF)
      authCheck rq = do
        eResult <- liftIO $ runHandler (authHandler' rq)
        case eResult of
          Left err -> delayedFailFatal err
          Right (SomeRole sr authR) ->
            case decideRole (sing @req) sr of
              Just proof -> pure (Satisfies proof authR)
              Nothing ->
                delayedFail
                  err403
                    { errBody = "Forbidden: insufficient permissions",
                      errHeaders = [("Content-Type", "text/plain; charset=utf-8")]
                    }
