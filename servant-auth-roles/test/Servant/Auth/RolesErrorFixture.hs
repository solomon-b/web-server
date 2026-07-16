{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-top-binds -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

-- | Fixture for the "forgot to run a deriver" regression test. Compiled with
-- @-fdefer-type-errors@ so the deliberately-missing 'Decidable' instance becomes
-- a runtime 'Control.Exception.TypeError' the spec can catch and inspect,
-- instead of a compile error that would fail the build.
module Servant.Auth.RolesErrorFixture (underivedDecidable) where

import Servant.Auth.Roles.TH

data Undrived = Foo | Bar

-- Singletons and ActualK are supplied by hand so the only thing missing is the
-- Decidable instance. This isolates our error from unrelated singletons errors.
$(genSingletons [''Undrived])

type instance ActualK (r :: Undrived) = Undrived

-- No deriveOrdRole/deriveEqRole/deriveMemberRole was run, so there is no Decidable
-- instance and the OVERLAPPABLE fallback's TypeError applies. Forcing this value
-- (which projects the missing dictionary) raises that error at runtime.
underivedDecidable :: ()
underivedDecidable = decideRole (sing @'Foo) (sing @'Foo) `seq` ()
