{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell derivers that make a role type usable with 'RequireRole'.
-- Given a plain @data@ declaration, they generate the singletons, the 'Proof'
-- data instance, and the 'Decidable' instance. No @deriving@ clause and no
-- singletons knowledge required.
--
-- @
-- import Servant.Auth.Roles.TH
--
-- data UserRole   = Viewer | Editor | Admin
-- data Region     = US | EU | APAC
-- data Permission = CanRead | CanWrite | CanManage
--
-- \$(deriveOrdRole    ''UserRole)     -- hierarchical   (actual >= required, via Ord)
-- \$(deriveEqRole     ''Region)       -- flat/exact     (actual == required, via Eq)
-- \$(deriveMemberRole ''Permission)   -- permission-set (required is a member)
-- @
--
-- A single @import Servant.Auth.Roles.TH@ brings the combinator, the classes, the
-- derivers, and the (unqualified) singletons Prelude the generated code needs.
module Servant.Auth.Roles.TH
  ( deriveOrdRole,
    deriveEqRole,
    deriveMemberRole,
    module Servant.Auth.Roles,
    module Data.Singletons.Base.TH,
  )
where

import Data.Kind (Type)
import Data.Singletons.Base.TH
import Language.Haskell.TH qualified as TH
import Servant.Auth.Roles

--------------------------------------------------------------------------------

-- | Hierarchical roles. Sufficiency is @required <= actual@ under the type's
-- (derived) 'Ord'. Generates a proof constructor named @\<TypeName\>Proof@.
deriveOrdRole :: TH.Name -> TH.Q [TH.Dec]
deriveOrdRole n = do
  sings <- concat <$> sequence [genSingletons [n], singEqInstances [n], singOrdInstances [n]]
  let k = TH.conT n
      con = TH.mkName (TH.nameBase n ++ "Proof")
  req <- TH.newName "req"
  r <- TH.newName "r"
  proofDec <-
    TH.dataInstD
      (pure [])
      ''Proof
      [[t|$(TH.varT req) :: $k|], [t|$(TH.varT r) :: $k|]]
      Nothing
      [ TH.forallC
          []
          (TH.cxt [[t|($(TH.varT req) <= $(TH.varT r)) ~ 'True|]])
          (TH.gadtC [con] [] [t|Proof $(TH.varT req) $(TH.varT r)|])
      ]
      []
  decidable <-
    [d|
      type instance ActualK (rq :: $k) = $k

      instance Decidable (rq :: $k) where
        decideRole a b = case a %<= b of
          STrue -> Just $(TH.conE con)
          SFalse -> Nothing
      |]
  packer <- mkPacker n k
  aliases <- mkAtleastAliases n
  pure (sings ++ proofDec : decidable ++ packer ++ aliases)

-- | Flat, unordered roles. Sufficiency is exact equality (@required == actual@)
-- via decidable equality. Generates a proof constructor @\<TypeName\>Proof@ and,
-- per constructor, an alias @type IsAdmin r = (r ~ 'Admin)@.
deriveEqRole :: TH.Name -> TH.Q [TH.Dec]
deriveEqRole n = do
  sings <- concat <$> sequence [genSingletons [n], singDecideInstances [n]]
  let k = TH.conT n
      con = TH.mkName (TH.nameBase n ++ "Proof")
  req <- TH.newName "req"
  r <- TH.newName "r"
  proofDec <-
    TH.dataInstD
      (pure [])
      ''Proof
      [[t|$(TH.varT req) :: $k|], [t|$(TH.varT r) :: $k|]]
      Nothing
      [ TH.forallC
          []
          (TH.cxt [[t|$(TH.varT req) ~ $(TH.varT r)|]])
          (TH.gadtC [con] [] [t|Proof $(TH.varT req) $(TH.varT r)|])
      ]
      []
  decidable <-
    [d|
      type instance ActualK (rq :: $k) = $k

      instance Decidable (rq :: $k) where
        decideRole a b = case a %~ b of
          Proved Refl -> Just $(TH.conE con)
          Disproved _ -> Nothing
      |]
  packer <- mkPacker n k
  aliases <- mkIsAliases n
  pure (sings ++ proofDec : decidable ++ packer ++ aliases)

-- | Permission-set roles. Two requirement forms are generated:
--
--   * A single required constructor, sufficient when it is a member of the held
--     set (@[Role]@). Membership witnesses are @\<TypeName\>Here@ and
--     @\<TypeName\>There@.
--   * A required /list/ of constructors, sufficient when every one is a member
--     of the held set (subset). This lets one gate demand several permissions:
--     @RequireRole "tag" '[CanRead, CanWrite]@. The subset witnesses are
--     @\<TypeName\>AllNil@ (empty requirement, vacuously satisfied) and
--     @\<TypeName\>AllCons@ (head present, rest present); an @AllCons@ carries
--     one element membership 'Proof' per required permission.
--
-- Also generates one constraint alias per constructor, @type Has\<Con\> ps =
-- Member 'Con ps@ (e.g. @HasCanWrite@), so a permission-gated function can name
-- the membership constraint. Release it in a handler with 'withMember' (single
-- permission) or 'withAllMembers' (a required list).
deriveMemberRole :: TH.Name -> TH.Q [TH.Dec]
deriveMemberRole n = do
  sings <- concat <$> sequence [genSingletons [n], singDecideInstances [n]]
  let p = TH.conT n
      hereC = TH.mkName (TH.nameBase n ++ "Here")
      thereC = TH.mkName (TH.nameBase n ++ "There")
      allNilC = TH.mkName (TH.nameBase n ++ "AllNil")
      allConsC = TH.mkName (TH.nameBase n ++ "AllCons")
      strict = TH.bangType (TH.bang TH.noSourceUnpackedness TH.noSourceStrictness)
  req <- TH.newName "req"
  ps <- TH.newName "ps"
  hd <- TH.newName "hd"
  rest <- TH.newName "rest"
  proofDec <-
    TH.dataInstD
      (pure [])
      ''Proof
      [[t|$(TH.varT req) :: $p|], [t|$(TH.varT ps) :: [$p]|]]
      Nothing
      [ TH.gadtC [hereC] [] [t|Proof $(TH.varT req) ($(TH.varT req) ': $(TH.varT ps))|],
        TH.gadtC
          [thereC]
          [strict [t|Proof $(TH.varT req) $(TH.varT rest)|]]
          [t|Proof $(TH.varT req) ($(TH.varT hd) ': $(TH.varT rest))|]
      ]
      []
  decidable <-
    [d|
      type instance ActualK (rq :: $p) = [$p]

      instance Decidable (rq :: $p) where
        decideRole rr pss = case pss of
          SNil -> Nothing
          SCons h t -> case rr %~ h of
            Proved Refl -> Just $(TH.conE hereC)
            Disproved _ -> $(TH.conE thereC) <$> decideRole rr t
      |]
  -- Subset form: a required @[Role]@ is satisfied when every element is a
  -- member of the held set. The proof is one element membership witness per
  -- required permission, so @decideRole@ over the required list reuses the
  -- single-permission @decideRole@ above (resolved by the element's kind).
  subReq <- TH.newName "req"
  subPs <- TH.newName "ps"
  hdR <- TH.newName "hd"
  restR <- TH.newName "rest"
  subProofDec <-
    TH.dataInstD
      (pure [])
      ''Proof
      [[t|$(TH.varT subReq) :: [$p]|], [t|$(TH.varT subPs) :: [$p]|]]
      Nothing
      [ TH.gadtC [allNilC] [] [t|Proof ('[] :: [$p]) $(TH.varT subPs)|],
        TH.gadtC
          [allConsC]
          [ strict [t|Proof $(TH.varT hdR) $(TH.varT subPs)|],
            strict [t|Proof $(TH.varT restR) $(TH.varT subPs)|]
          ]
          [t|Proof ($(TH.varT hdR) ': $(TH.varT restR)) $(TH.varT subPs)|]
      ]
      []
  subDecidable <-
    [d|
      type instance ActualK (rq :: [$p]) = [$p]

      instance Decidable (rq :: [$p]) where
        decideRole sreq held = case sreq of
          SNil -> Just $(TH.conE allNilC)
          SCons h t -> $(TH.conE allConsC) <$> decideRole h held <*> decideRole t held
      |]
  packer <- mkPacker n [t|[$p]|]
  aliases <- mkHasAliases n
  pure (sings ++ proofDec : decidable ++ subProofDec : subDecidable ++ aliases ++ packer)

--------------------------------------------------------------------------------

-- | Generate a singleton-free packer @some\<TypeName\>@ that reflects a runtime
-- role into the type index and packs it as 'SomeRole'. The @toSing@/@SomeSing@
-- work stays in the body, and the signature names no singleton types, so an
-- auth handler that uses it needs no singletons. @kk@ is the index kind
-- (@UserRole@ for 'deriveOrdRole', @[Permission]@ for 'deriveMemberRole').
mkPacker :: TH.Name -> TH.Q TH.Type -> TH.Q [TH.Dec]
mkPacker n kk = do
  let packerName = TH.mkName ("some" ++ TH.nameBase n)
  role <- TH.newName "role"
  val <- TH.newName "val"
  sig <-
    TH.sigD
      packerName
      [t|forall (authF :: $kk -> Type). $kk -> (forall (r :: $kk). authF r) -> SomeRole authF|]
  def <-
    TH.funD
      packerName
      [ TH.clause
          [TH.varP role, TH.varP val]
          (TH.normalB [|case toSing $(TH.varE role) of SomeSing sr -> SomeRole sr $(TH.varE val)|])
          []
      ]
  pure [sig, def]

-- | Generate one constraint alias per constructor, @type \<prefix\>\<Con\> r = ...@,
-- so handlers can name the proof (@IsAtleastAdmin r =>@ or @IsAdmin r =>@) instead
-- of writing the raw type-level relation. @body@ receives the promoted
-- constructor and the role variable.
mkAliases :: String -> (TH.Q TH.Type -> TH.Q TH.Type -> TH.Q TH.Type) -> TH.Name -> TH.Q [TH.Dec]
mkAliases prefix body n = do
  cons <- roleConstructors n
  traverse alias cons
  where
    alias c = do
      r <- TH.newName "r"
      TH.tySynD (TH.mkName (prefix ++ TH.nameBase c)) [TH.plainTV r] (body (TH.promotedT c) (TH.varT r))

-- | Aliases for the Ord scheme, @type IsAtleastAdmin r = ('Admin <= r) ~ 'True@.
mkAtleastAliases :: TH.Name -> TH.Q [TH.Dec]
mkAtleastAliases = mkAliases "IsAtleast" (\c r -> [t|($c <= $r) ~ 'True|])

-- | Aliases for the Eq scheme, @type IsAdmin r = (r ~ 'Admin)@.
mkIsAliases :: TH.Name -> TH.Q [TH.Dec]
mkIsAliases = mkAliases "Is" (\c r -> [t|$r ~ $c|])

-- | Aliases for the member scheme, @type HasCanWrite ps = Member 'CanWrite ps@.
-- Names the membership constraint a permission-gated function demands.
mkHasAliases :: TH.Name -> TH.Q [TH.Dec]
mkHasAliases = mkAliases "Has" (\c r -> [t|Member $c $r|])

-- | The data constructors of a single @data@ declaration.
roleConstructors :: TH.Name -> TH.Q [TH.Name]
roleConstructors n = do
  info <- TH.reify n
  case info of
    TH.TyConI (TH.DataD _ _ _ _ cons _) -> pure (concatMap conNames cons)
    _ -> fail ("Servant.Auth.Roles.TH: expected a data type, got " ++ show n)
  where
    conNames (TH.NormalC c _) = [c]
    conNames (TH.RecC c _) = [c]
    conNames (TH.InfixC _ c _) = [c]
    conNames (TH.GadtC cs _ _) = cs
    conNames (TH.RecGadtC cs _ _) = cs
    conNames (TH.ForallC _ _ con) = conNames con
