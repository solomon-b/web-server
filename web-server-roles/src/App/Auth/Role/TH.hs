{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell derivers that make a role type usable with 'RequireRole'.
-- Given a plain @data@ declaration, they generate the singletons, the 'Proof'
-- data instance, and the 'Decidable' instance. No @deriving@ clause and no
-- singletons knowledge required.
--
-- @
-- import App.Auth.Role.TH
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
-- A single @import App.Auth.Role.TH@ brings the combinator, the classes, the
-- derivers, and the (unqualified) singletons Prelude the generated code needs.
module App.Auth.Role.TH
  ( deriveOrdRole,
    deriveEqRole,
    deriveMemberRole,
    module App.Auth.Role,
    module Data.Singletons.Base.TH,
  )
where

import App.Auth.Role
import Data.Kind (Type)
import Data.Singletons.Base.TH
import Language.Haskell.TH qualified as TH

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

-- | Permission-set roles. The required constructor must be a member of the held
-- set (@[Role]@). Generates membership witnesses @\<TypeName\>Here@ and
-- @\<TypeName\>There@.
deriveMemberRole :: TH.Name -> TH.Q [TH.Dec]
deriveMemberRole n = do
  sings <- concat <$> sequence [genSingletons [n], singDecideInstances [n]]
  let p = TH.conT n
      hereC = TH.mkName (TH.nameBase n ++ "Here")
      thereC = TH.mkName (TH.nameBase n ++ "There")
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
          [TH.bangType (TH.bang TH.noSourceUnpackedness TH.noSourceStrictness) [t|Proof $(TH.varT req) $(TH.varT rest)|]]
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
  packer <- mkPacker n [t|[$p]|]
  pure (sings ++ proofDec : decidable ++ packer)

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

-- | The data constructors of a single @data@ declaration.
roleConstructors :: TH.Name -> TH.Q [TH.Name]
roleConstructors n = do
  info <- TH.reify n
  case info of
    TH.TyConI (TH.DataD _ _ _ _ cons _) -> pure (concatMap conNames cons)
    _ -> fail ("App.Auth.Role.TH: expected a data type, got " ++ show n)
  where
    conNames (TH.NormalC c _) = [c]
    conNames (TH.RecC c _) = [c]
    conNames (TH.InfixC _ c _) = [c]
    conNames (TH.GadtC cs _ _) = cs
    conNames (TH.RecGadtC cs _ _) = cs
    conNames (TH.ForallC _ _ con) = conNames con
