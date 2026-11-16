{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.GenDefs
Description : Generated type definitions read from their declarations (GEN-9)

A generated record is defined once per language from its declaration, not
from its uses. The definition is the record applied to its own parameters,
each parameter standing for itself: a placeholder type whose native form
is the parameter's native name. A use is then an instance of the
definition, and its arguments are found by matching the definition's fields
against the use's fields.
-}
module Morloc.CodeGenerator.GenDefs
  ( GenDef (..)
  , genDefs
  , inferArgs
  , instantiate
  , checkUse
  , Disagreement (..)
  , canon
  , RecordUse
  , recordUses
  , disagreements
  , intrinsicSerialASTs
  , placeTypes
  , templateArgs
  , recordArgs
  , useArgs
  , placeholders
  , nativeParam
  , nativeParams
  ) where

import qualified Data.Text as T
import Morloc.CodeGenerator.Infer (getScope, inferConcreteType)
import Morloc.CodeGenerator.Namespace
import qualified Data.Set as Set
import Morloc.CodeGenerator.Grammars.Common (intrinsicValueTypes, recordsInType)
import Morloc.CodeGenerator.Grammars.Macro (expandMacro)
import Morloc.CodeGenerator.Serial (makeSerialAST)
import qualified Morloc.Data.Map as Map
import Morloc.Data.Zip (zipSame, zipWithSame)
import qualified Morloc.Monad as MM
import Control.Monad.Except (catchError)
import Morloc.Data.Doc (render)

-- | A generated record as its declaration defines it in one language.
data GenDef = GenDef
  { gdVar :: TVar
  , gdForm :: T.Text
  -- ^ the native form: @struct@ for a generated record, else the user's
  , gdParams :: [(TVar, Kind)]
  -- ^ declaration order
  , gdFields :: [(Key, TypeF)]
  -- ^ each parameter appears as its placeholder ('paramOf'); a reference
  -- to the record itself is a 'RecF'
  }
  deriving (Show)

-- | The placeholder for parameter @p@: a name no program can write, so it
-- never meets a user's type.
placeholder :: TVar -> TVar
placeholder p = TV ("&" <> unTVar p)

-- | The parameter a placeholder stands for.
paramOf :: TypeF -> Maybe TVar
paramOf (VarF (FV (TV n) _)) = TV <$> T.stripPrefix "&" n
paramOf _ = Nothing

-- | The declaration-driven definition of every used record, generated or
-- user-mapped, in the given language: its form for any arguments (a
-- specialisation for particular arguments is not it). Parameters are
-- spelled @T_<p>@. A record whose declaration cannot be resolved on its own
-- is left out.
genDefs :: Lang -> Set.Set TVar -> MorlocMonad (Map.Map TVar GenDef, Map.Map TVar T.Text)
genDefs lang used = do
  (cscope, _) <- getScope lang
  let decls =
        [ (gv, form, ps)
        | (gv, entries) <- Map.toList cscope
        , Set.member gv used
        , (form, ps) : _ <-
            [ [ (unTVar n, ps)
              | (ps0, NamU _ n _ _, _, _, _) <- entries
              , Just ps <- [mapM (either Just (const Nothing)) ps0]
              ]
            ]
        ]
  results <- mapM one decls
  return (Map.fromList [(gv, gd) | (gv, Right gd) <- results], Map.fromList [(gv, e) | (gv, Left e) <- results])
  where
    one (gv, form, ps) = do
      st0 <- MM.get
      let entry body = [([], body, ArgDocAlias defaultValue, True, TypedefAlias)]
          added = Map.fromList [(placeholder p, entry (VarU (TV (nativeParam p)))) | (p, KindType) <- ps]
          t = if null ps then VarT gv else AppT (VarT gv) (map paramArg ps)
      MM.modify $ \st -> st {stateConcreteTypedefs = Map.adjust (Map.union added) lang (stateConcreteTypedefs st)}
      -- A declaration the language cannot resolve on its own (a form that
      -- needs arguments the declaration does not fix) has no definition,
      -- and a use of the record says why.
      r <-
        ( ( \tf -> case tf of
              NamF _ _ args rs -> (gv, Right (GenDef gv form ps [(k, foldSelf gv args x) | (k, x) <- rs]))
              _ -> (gv, Left "its declaration does not resolve to a record")
          )
            <$> inferConcreteType lang (Idx 0 t)
        )
          `catchRejected` (\msg -> return (gv, Left msg))
      MM.put st0
      return r
    paramArg (p, KindType) = TyA (VarT (placeholder p))
    paramArg (p, _) = KiA (KVarT p)

-- | Run an action, and on a rejection of the program return its message.
catchRejected :: MorlocMonad a -> (T.Text -> MorlocMonad a) -> MorlocMonad a
catchRejected act h =
  act `catchError` \e -> case e of
    Rejected _ d -> h (render d)
    _ -> MM.throwError e

-- | Fold the record's own occurrences into back-references: those at the
-- arguments of the enclosing occurrence. One at other arguments (the inner
-- `Pair Int` of a `Pair (Pair Int)`) is another type.
foldSelf :: TVar -> [ArgF] -> TypeF -> TypeF
foldSelf self ps0 t = case t of
  NamF _ (FV g _) ps _ | g == self -> if ps == ps0 then back else t
  RecF (FV g _) | g == self -> back
  VarF (FV g _) | g == self -> back
  AppF (VarF (FV g _)) ps | g == self -> if ps == ps0 then back else t
  _ -> mapChildren (foldSelf self ps0) t
  where
    back = RecF (FV self (CV "struct"))

-- | A type with each of its immediate component types mapped.
mapChildren :: (TypeF -> TypeF) -> TypeF -> TypeF
mapChildren go t = case t of
  NamF o v ps rs -> NamF o v (map (fmap go) ps) [(k, go x) | (k, x) <- rs]
  FunF as r -> FunF (map go as) (go r)
  AppF h as -> AppF (go h) (map (fmap go) as)
  OptionalF x -> OptionalF (go x)
  VariantF v ps as -> VariantF v (map (fmap go) ps) [(c, map go ts) | (c, ts) <- as]
  EnumF v ps cs -> EnumF v (map (fmap go) ps) cs
  _ -> t

-- | The arguments of a use, by parameter: each placeholder in the
-- definition's fields is bound to the use's type at the same position. A
-- nested generated record is matched through its fields, never through
-- the arguments it carries, which on the wire side are still the native
-- ones. Positions whose shapes differ bind nothing; the agreement check
-- reports them.
inferArgs :: Map.Map TVar GenDef -> GenDef -> [(Key, TypeF)] -> Either T.Text (Map.Map TVar TypeF)
inferArgs defs gd use = do
  args <- inferArgsPartial defs gd use
  case [p | (p, KindType) <- gdParams gd, Map.notMember p args] of
    [] -> Right args
    (p : _) -> Left ("the parameter " <> unTVar p <> " is not determined by the fields of this use")

-- | The arguments a use's fields bind; a parameter they do not bind is
-- absent.
inferArgsPartial :: Map.Map TVar GenDef -> GenDef -> [(Key, TypeF)] -> Either T.Text (Map.Map TVar TypeF)
inferArgsPartial defs gd use = do
  unless inOrder (Left "its fields are in another order than the declaration's")
  binds <- concat <$> mapM field (gdFields gd)
  let grouped = Map.fromListWith (flip (<>)) [(p, [t]) | (p, t) <- binds]
  Map.traverseWithKey one grouped
  where
    field (k, d) = case lookup k use of
      Just u -> Right (match d u)
      Nothing -> Left ("the field " <> unKey k <> " is missing from a use")
    -- The definition's marshalling node writes field i to slot i of the
    -- use's schema, so the fields must come in the same order.
    inOrder = map fst use == map fst (gdFields gd)
    -- Two bindings agree when they are one type: a recursive record
    -- unrolled to different depths is still one record.
    one p (t : ts)
      | all (== t) ts = Right t
      | let c = canon defs t, all ((== c) . canon defs) ts = Right t
      | otherwise = Left ("the parameter " <> unTVar p <> " is bound to different types")
    one p [] = Left ("the parameter " <> unTVar p <> " is unbound")

    match :: TypeF -> TypeF -> [(TVar, TypeF)]
    match d u
      | Just p <- paramOf d = [(p, u)]
    -- The arguments a nested record carries are never read: on the wire
    -- side they are the native ones. A parameter reached only through them
    -- is undetermined, and the use is refused.
    match (NamF _ (FV g _) _ drs) (NamF _ (FV g' _) _ urs)
      | g == g' = concat [match dt ut | (k, dt) <- drs, Just ut <- [lookup k urs]]
    match (AppF dh@(VarF (FV g _)) das) (AppF uh@(VarF (FV g' _)) uas)
      | g == g' = match dh uh <> matchArgs das uas
    match (FunF das dr) (FunF uas ur)
      | length das == length uas = concat (zipWithSame match das uas) <> match dr ur
    match (OptionalF d) (OptionalF u) = match d u
    -- A `data` type is matched through its arms only: its arguments, like
    -- a record's, are the native ones on the wire side, and an argument no
    -- arm holds has no native form.
    match (VariantF (FV g _) _ das) (VariantF (FV g' _) _ uas)
      | g == g' =
          concat [concat (zipWithSame match dts uts) | length das == length uas, ((c, dts), (c', uts)) <- zipSame das uas, c == c', length dts == length uts]
    match _ _ = []

    matchArgs ds us
      | length ds == length us = concat [match d u | (TyA d, TyA u) <- zipSame ds us]
      | otherwise = []

-- | Replace each placeholder by its argument.
instantiate :: Map.Map TVar TypeF -> TypeF -> TypeF
instantiate args t
  | Just p <- paramOf t = Map.findWithDefault t p args
  | otherwise = mapChildren (instantiate args) t

-- | Whether a use agrees with its record's definition: Nothing, or the
-- first disagreement.
-- A reference of the definition to its record is the record at this
-- use's own arguments.
checkUse :: Map.Map TVar GenDef -> GenDef -> [ArgF] -> [(Key, TypeF)] -> Maybe Disagreement
checkUse defs gd carried rs = case inferArgsPartial defs gd rs of
  Left e -> Just (Unmatched e)
  Right args ->
    firstJust id
      [ if canon defs (selfAt d') == canon defs u then Nothing else Just (FieldDiffers k d' u)
      | (k, d) <- gdFields gd
      , let d' = instantiate args d
      , Just u <- [lookup k rs]
      ]
  where
    self = NamF NamRecord (FV (gdVar gd) (CV (gdForm gd))) carried rs
    selfAt t = case t of
      RecF (FV g _) | g == gdVar gd -> self
      _ -> mapChildren selfAt t

-- | Why a use is not an instance of its record's definition.
data Disagreement
  = Unmatched T.Text
  -- ^ its fields bind no consistent arguments
  | FieldDiffers Key TypeF TypeF
  -- ^ a field, as the definition has it at the use's arguments, and as
  -- the use has it
  deriving (Show)

-- | The canonical nominal form two types are compared in (GEN-9): a record
-- is its name applied to the arguments its fields bind ('recordArgs'),
-- with its fields dropped, so a recursive record unrolled to any depth is
-- one form, and a back-reference is the record at no arguments. A record
-- with no definition keeps its fields.
canon :: Map.Map TVar GenDef -> TypeF -> TypeF
canon defs t = case t of
  NamF _ (FV g cv) ps rs
    | Just gd <- Map.lookup g defs
    , Right as <- recordArgs defs gd ps rs ->
        NamF NamRecord (FV g cv) (map (TyA . canon defs) as) []
    | otherwise -> NamF NamRecord (FV g cv) (map (fmap (canon defs)) ps) [(k, canon defs x) | (k, x) <- rs]
  RecF (FV g _)
    | Just gd <- Map.lookup g defs -> NamF NamRecord (FV g (CV (gdForm gd))) [] []
  -- A reference to a record by its name alone is the record.
  VarF (FV g _)
    | Just gd <- Map.lookup g defs -> NamF NamRecord (FV g (CV (gdForm gd))) [] []
  AppF (VarF (FV g _)) as
    | Just gd <- Map.lookup g defs -> NamF NamRecord (FV g (CV (gdForm gd))) (map (fmap (canon defs)) as) []
  _ -> mapChildren (canon defs) t

-- | The parameters a type holds in a field, the ones its uses can bind:
-- the arguments a nested record or `data` type carries are not counted,
-- since its fields say what it holds.
placeholders :: TypeF -> [TVar]
placeholders t
  | Just p <- paramOf t = [p]
placeholders t = case t of
  NamF _ _ _ rs -> concatMap (placeholders . snd) rs
  FunF as r -> concatMap placeholders (r : as)
  -- A native form that does not spell its arguments holds none of them.
  AppF h@(VarF (FV _ (CV form))) _
    | not (T.any (== '$') form) -> placeholders h
  AppF h as -> placeholders h <> concatMap placeholders (tyArgs as)
  OptionalF x -> placeholders x
  VariantF _ _ as -> concatMap (concatMap placeholders . snd) as
  _ -> []

-- | The parameters a record's native form takes (GEN-9): its type
-- parameters some field holds, in order. Any other parameter is erased.
nativeParams :: GenDef -> [TVar]
nativeParams gd = [p | (p, KindType) <- gdParams gd, Set.member p held]
  where
    held = Set.fromList (concatMap (placeholders . snd) (gdFields gd))

-- | The native name of a parameter (D14).
nativeParam :: TVar -> T.Text
nativeParam p = "T_" <> unTVar p

-- | An occurrence of a record: its name, its native form, the arguments
-- it carries and its fields.
type RecordUse = (TVar, T.Text, [ArgF], [(Key, TypeF)])

-- | The occurrences of records in the given types, at any depth.
recordUses :: [TypeF] -> [RecordUse]
recordUses ts = unique [(gv, cv, ps, rs) | (FV gv (CV cv), ps, rs) <- concatMap recordsInType ts]

-- | Each use of a generated record that is not an instance of its
-- definition (GEN-9), with why.
disagreements :: Map.Map TVar GenDef -> [RecordUse] -> [(TVar, Disagreement)]
disagreements defs uses =
  [ (gv, d)
  | (gv, cv, ps, rs) <- uses
  , cv == "struct"
  , Just gd <- [Map.lookup gv defs]
  , Just d <- [checkUse defs gd ps rs]
  ]

-- | The type arguments a record use is spelled at, in order (GEN-9): a
-- parameter its fields bind takes that binding; one no field binds takes
-- the argument the use carries, since no value depends on it. The carried
-- arguments are otherwise never read: on the unpacked side of a packed
-- argument they are the native ones.
recordArgs :: Map.Map TVar GenDef -> GenDef -> [ArgF] -> [(Key, TypeF)] -> Either T.Text [TypeF]
recordArgs defs gd carried rs = do
  bound <- inferArgsPartial defs gd rs
  let typeParams = [p | (p, KindType) <- gdParams gd]
      given = tyArgs carried
  if length given /= length typeParams
    then Left ("it carries " <> T.pack (show (length given)) <> " type arguments for " <> T.pack (show (length typeParams)) <> " parameters")
    else Right [Map.findWithDefault c p bound | (p, c) <- zipSame typeParams given]

-- | The arguments a use of a user-mapped record is spelled at: its carried
-- arguments with each type argument its fields bind replaced by that
-- binding ('recordArgs'). A use at a specialised form (a form other than
-- the definition's) keeps its carried arguments, as does one whose fields
-- do not determine a binding.
useArgs :: Map.Map TVar GenDef -> TVar -> T.Text -> [ArgF] -> [(Key, TypeF)] -> [ArgF]
useArgs defs gv form carried rs = case Map.lookup gv defs of
  Just gd | gdForm gd == form, Right ts <- recordArgs defs gd carried rs -> refill ts carried
  _ -> carried
  where
    refill ts (TyA _ : rest) | (t : ts') <- ts = TyA t : refill ts' rest
    refill ts (a@(KiA _) : rest) = a : refill ts rest
    refill _ rest = rest

-- | A use's arguments, in the definition's parameter order.
templateArgs :: Map.Map TVar GenDef -> GenDef -> [(Key, TypeF)] -> Either T.Text [TypeF]
templateArgs defs gd rs = do
  args <- inferArgsPartial defs gd rs
  mapM (\p -> maybe (Left ("the parameter " <> unTVar p <> " is not determined")) Right (Map.lookup p args)) (nativeParams gd)


-- | The serial forms of the values a pool's intrinsics marshal, so that
-- every instance the pool writes is known (GEN-14). A value with no serial
-- form is one no intrinsic can marshal either, and the intrinsic reports it.
intrinsicSerialASTs :: Lang -> [SerialManifold] -> MorlocMonad [SerialAST]
intrinsicSerialASTs lang es =
  concat <$> mapM (\t -> MM.recoverRejected ((: []) <$> makeSerialAST 0 lang t) (return [])) (unique (concatMap intrinsicValueTypes es))

-- | Which generated types a language's files can hold (GEN-10): of the
-- candidates, those whose fields name only parameters, generated types the
-- files hold, and native forms the language accepts there. A type that
-- names one the files do not hold stays in the pool, which can move
-- another into the pool, so this is a fixed point. It reads the
-- declarations alone.
placeTypes ::
  (T.Text -> Bool) ->
  -- ^ whether a file may name a native form (its `$N` slots removed)
  Set.Set TVar ->
  -- ^ every generated type, public or not
  Map.Map TVar [TypeF] ->
  -- ^ the field types of each candidate
  Set.Set TVar
placeTypes leafOk generated fieldsOf = go (Map.keysSet fieldsOf)
  where
    go pub =
      let pub' = Set.filter (\g -> all (safe pub) (Map.findWithDefault [] g fieldsOf)) pub
       in if pub' == pub then pub else go pub'
    -- A native form's `$N` slots hold the arguments, checked on their own.
    form f = leafOk (expandMacro f (repeat ""))
    -- A type named by a native form, or a generated one the files hold.
    nominal pub g f ps
      | Set.member g generated = Set.member g pub
      | otherwise = form f && all (safe pub) (tyArgs ps)
    safe _ t
      | isJust (paramOf t) = True
    safe pub t = case t of
      NamF NamTable _ _ _ -> False
      NamF _ (FV g (CV "struct")) _ _ -> Set.member g pub
      NamF _ (FV g (CV f)) ps _ -> nominal pub g f ps
      RecF (FV g _) -> Set.member g pub
      VariantF (FV g (CV f)) ps _ -> nominal pub g f ps
      EnumF (FV g (CV f)) ps _ -> nominal pub g f ps
      VarF (FV g (CV f)) -> nominal pub g f []
      AppF (VarF (FV _ (CV f))) as -> form f && all (safe pub) (tyArgs as)
      AppF h as -> safe pub h && all (safe pub) (tyArgs as)
      OptionalF x -> safe pub x
      FunF as r -> all (safe pub) (r : as)
      UnkF _ -> False
