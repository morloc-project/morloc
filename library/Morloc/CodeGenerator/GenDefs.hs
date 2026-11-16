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
  , DataDef (..)
  , dataDefs
  , inferArgs
  , instantiate
  , checkUse
  , Disagreement (..)
  , canon
  , untiedSelf
  , RecordUse
  , recordUses
  , disagreements
  , inPoolSerialASTs
  , placeTypes
  , templateArgs
  , carriedTemplateArgs
  , nestedSelfUses
  , recordArgs
  , useArgs
  , placeholders
  , nativeParam
  , nativeParams
  , nativeDataParams
  , armDataParams
  , dataUseArgs
  , dataNamesIn
  ) where

import qualified Data.Text as T
import Morloc.CodeGenerator.Infer (concreteNameOf, getScope, heldDataParams, inferConcreteType)
import Morloc.CodeGenerator.Namespace
import qualified Data.Set as Set
import Morloc.CodeGenerator.Grammars.Common (intrinsicValueTypes, poolNativeTypes, recordsInType, serialClosuresOf)
import Morloc.CodeGenerator.Grammars.Macro (expandMacro)
import Morloc.CodeGenerator.Serial (makeSerialAST)
import qualified Morloc.Data.Map as Map
import Morloc.Data.Zip (indexed, indexedFrom, zipExact, zipPrefix, zipSame, zipWithSame)
import qualified Morloc.Monad as MM
import Control.Monad.Except (catchError)
import Morloc.Data.Doc (pretty, render)
import Data.Char (isDigit)
import Text.Read (readMaybe)

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
      -- A declaration the language cannot resolve on its own (a form that
      -- needs arguments the declaration does not fix) has no definition,
      -- and a use of the record says why.
      r <-
        ( ( \tf -> case tf of
              NamF o fv args rs -> (gv, Right (NamF o fv args [(k, foldSelf fv args x) | (k, x) <- rs]))
              _ -> (gv, Left "its declaration does not resolve to a record")
          )
            <$> atPlaceholders lang gv ps
        )
          `catchRejected` (\msg -> return (gv, Left msg))
      case r of
        (_, Right t@(NamF _ _ _ rs)) -> case untiedSelf t of
          ((g, True) : _) -> MM.throwCompilerBug ("a reference of the record " <> pretty g <> " to itself was not tied in inference")
          -- A reference to the record at other arguments is another
          -- instance of it, which is not generated yet.
          ((g, False) : _) ->
            return (gv, Left ("it refers to " <> unTVar g <> " at other arguments than its own, which is not generated yet"))
          [] -> return (gv, Right (GenDef gv form ps rs))
        (_, Right _) -> return (gv, Left "its declaration does not resolve to a record")
        (_, Left e) -> return (gv, Left e)

-- | A declared type applied to its parameters' placeholders, resolved in a
-- language: its definition for any arguments. The state is as before.
atPlaceholders :: Lang -> TVar -> [(TVar, Kind)] -> MorlocMonad TypeF
atPlaceholders lang gv ps = do
  st0 <- MM.get
  let entry body = [([], body, ArgDocAlias defaultValue, True, TypedefAlias)]
      added = Map.fromList [(placeholder p, entry (VarU (TV (nativeParam p)))) | (p, KindType) <- ps]
      t = if null ps then VarT gv else AppT (VarT gv) (map paramArg ps)
  MM.modify $ \st -> st {stateConcreteTypedefs = Map.adjust (Map.union added) lang (stateConcreteTypedefs st)}
  r <- inferConcreteType lang (Idx 0 t) `catchError` (\e -> MM.put st0 >> MM.throwError e)
  MM.put st0
  return r
  where
    paramArg (p, KindType) = TyA (VarT (placeholder p))
    paramArg (p, _) = KiA (KVarT p)

-- | A generated parameterized `data` type as its declaration defines it in
-- one language: its native form, a template over the parameters its arms
-- hold ('nativeDataParams'), and its arms with each parameter as its
-- placeholder.
data DataDef = DataDef
  { ddVar :: TVar
  , ddForm :: T.Text
  , ddTypeParams :: [TVar]
  -- ^ its type parameters, in order: the positions of a use's arguments
  , ddNative :: [TVar]
  -- ^ the parameters of its native template
  , ddArms :: [(T.Text, [TypeF])]
  }
  deriving (Show)

-- | The parameters a `data` definition's native form takes: its type
-- parameters some arm holds, in declaration order (Infer.heldDataParams).
nativeDataParams :: DataDef -> [TVar]
nativeDataParams = ddNative

-- | The native parameters one arm of a definition holds, in order: the
-- parameters of a language's per-arm type where one may not take more.
armDataParams :: DataDef -> T.Text -> [TVar]
armDataParams dd c = [p | p <- ddNative dd, Set.member p held]
  where
    held = Set.fromList (concatMap placeholders (concat [ts | (c', ts) <- ddArms dd, c' == c]))

-- | The `data` types named anywhere in the types.
dataNamesIn :: [TypeF] -> Set.Set TVar
dataNamesIn = Set.fromList . concatMap go
  where
    go t = case t of
      VariantF (FV g _) _ _ -> g : concatMap go (childrenOf t)
      _ -> concatMap go (childrenOf t)

-- | The definitions of the given generated `data` types whose native form
-- is a template, and why each whose declaration cannot be resolved on its
-- own has none.
dataDefs :: Lang -> Set.Set TVar -> MorlocMonad (Map.Map TVar DataDef, Map.Map TVar T.Text)
dataDefs lang used = do
  gscope <- MM.getGeneralScope
  (cscope, _) <- getScope lang
  results <- mapM (one gscope) [gv | gv <- Set.toList used, concreteNameOf cscope gv == unTVar gv, not (null (heldDataParams gscope gv))]
  return (Map.fromList [(gv, dd) | Just (gv, Right dd) <- results], Map.fromList [(gv, e) | Just (gv, Left e) <- results])
  where
    one gscope gv = case [ps | (ps0, _, _, _, TypedefEnum) <- Map.findWithDefault [] gv gscope, Just ps <- [mapM (either Just (const Nothing)) ps0]] of
      (ps : _) ->
        ( ( \tf -> case tf of
              VariantF (FV _ (CV form)) _ arms | T.any (== '$') form -> Just (gv, Right (DataDef gv form [p | (p, KindType) <- ps] (heldDataParams gscope gv) arms))
              _ -> Nothing
          )
            <$> atPlaceholders lang gv ps
        )
          `catchRejected` (\msg -> return (Just (gv, Left msg)))
      [] -> return Nothing

-- | Run an action, and on a rejection of the program return its message.
catchRejected :: MorlocMonad a -> (T.Text -> MorlocMonad a) -> MorlocMonad a
catchRejected act h =
  act `catchError` \e -> case e of
    Rejected _ d -> h (render d)
    _ -> MM.throwError e

-- | Fold an unrolled copy of the record, an occurrence at the arguments of
-- the enclosing one, into a reference to it. One at other arguments (the
-- inner `Pair Int` of a `Pair (Pair Int)`) is another type.
foldSelf :: FVar -> [ArgF] -> TypeF -> TypeF
foldSelf fv@(FV self _) ps0 t = case t of
  NamF _ (FV g _) ps _ | g == self, ps == ps0 -> RecF fv ps0
  _ -> mapChildren (foldSelf fv ps0) t

-- | The generated records referred to by name inside an occurrence of
-- themselves, each with whether the reference is at the occurrence's own
-- arguments: such a reference inference should have tied ('RecF'); one at
-- other arguments names another instance. A record mapped to a native
-- form is named through it, at any arguments.
untiedSelf :: TypeF -> [(TVar, Bool)]
untiedSelf = go Map.empty
  where
    go anc t = case t of
      NamF _ (FV g (CV "struct")) ps rs -> concatMap (go (Map.insert g ps anc) . snd) rs
      VarF (FV g _) | Just ps <- Map.lookup g anc -> [(g, null ps)]
      AppF (VarF (FV g _)) as | Just ps <- Map.lookup g anc -> [(g, as == ps)]
      _ -> concatMap (go anc) (childrenOf t)

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
      [ if canonAt d' == canonAt u then Nothing else Just (FieldDiffers k d' u)
      | (k, d) <- gdFields gd
      , let d' = instantiate args d
      , Just u <- [lookup k rs]
      ]
  where
    self = NamF NamRecord (FV (gdVar gd) (CV (gdForm gd))) carried rs
    -- Inside the use, a reference to its record is the use itself.
    canonAt = canonIn defs (Map.singleton (gdVar gd, carried) (canon defs self))

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
canon defs = canonIn defs Map.empty

-- | 'canon', with the canonical forms of the record occurrences that
-- enclose the type, by name and arguments: a reference to one of them is
-- that occurrence.
canonIn :: Map.Map TVar GenDef -> Map.Map (TVar, [ArgF]) TypeF -> TypeF -> TypeF
canonIn defs env t = case t of
  NamF _ (FV g cv) ps rs
    | Just gd <- Map.lookup g defs
    , Right as <- recordArgs defs gd ps rs ->
        NamF NamRecord (FV g cv) (map (TyA . canonIn defs env) as) []
    | otherwise ->
        let ps' = map (fmap (canonIn defs env)) ps
            env' = Map.insert (g, ps) (NamF NamRecord (FV g cv) ps' []) env
         in NamF NamRecord (FV g cv) ps' [(k, canonIn defs env' x) | (k, x) <- rs]
  -- A `data` type is its instance: its name, which a generated type
  -- derives from its arguments, and the arguments a template spells. Its
  -- arms are dropped, so a reference to it from inside its own arms (a
  -- back-edge, which carries none) is the same type.
  VariantF fv@(FV _ (CV form)) ps _
    | T.any (== '$') form -> VariantF fv [TyA (canonIn defs env t) | (k, t) <- indexedFrom 1 (tyArgs ps), k `elem` macroSlots form] []
    | otherwise -> VariantF fv [] []
  RecF (FV g cv) ps
    | Just c <- Map.lookup (g, ps) env -> c
    | Just gd <- Map.lookup g defs -> NamF NamRecord (FV g (CV (gdForm gd))) (map (fmap (canonIn defs env)) ps) []
    | otherwise -> NamF NamRecord (FV g cv) (map (fmap (canonIn defs env)) ps) []
  _ -> mapChildren (canonIn defs env) t

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
  -- A template spells the arguments at its `$k` slots.
  VariantF (FV _ (CV form)) ps _
    | T.any (== '$') form -> concatMap placeholders [t | (k, t) <- indexedFrom 1 (tyArgs ps), k `elem` macroSlots form]
  VariantF _ _ as -> concatMap (concatMap placeholders . snd) as
  _ -> []

-- | The positions a native form's `$k` slots name.
macroSlots :: T.Text -> [Int]
macroSlots form = [k | piece <- drop 1 (T.splitOn "$" form), Just k <- [readMaybe (T.unpack (T.takeWhile isDigit piece))]]

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

-- | The arguments a use of a generated `data` template is spelled at: its
-- carried arguments with each one its arms bind replaced by that binding,
-- as a record's fields do ('useArgs'). An unpacked use carries the native
-- arguments, which name no unpacked value. A use with no arms (a
-- back-edge) keeps its carried arguments.
dataUseArgs :: Map.Map TVar GenDef -> DataDef -> [ArgF] -> [(T.Text, [TypeF])] -> [ArgF]
dataUseArgs defs dd carried arms
  | null arms = carried
  | otherwise = useArgs (Map.insert (ddVar dd) asRecord defs) (ddVar dd) (ddForm dd) carried (fields arms)
  where
    asRecord = GenDef (ddVar dd) (ddForm dd) [(p, KindType) | p <- ddTypeParams dd] (fields (ddArms dd))
    fields as = [(Key (c <> "." <> T.pack (show i)), t) | (c, ts) <- as, (i, t) <- indexed ts]

-- | A reference's arguments at its record's native parameters: those it
-- carries, which on the native side are its own ('RecF' has no fields to
-- bind them).
carriedTemplateArgs :: GenDef -> [ArgF] -> Maybe [TypeF]
carriedTemplateArgs gd ps = do
  pairs <- zipExact [p | (p, KindType) <- gdParams gd] (tyArgs ps)
  return [t | (p, t) <- pairs, p `elem` nativeParams gd]

-- | The recursive parameterized records used at an argument that holds the
-- record itself (`Container (Container Int)`), which are not generated:
-- the inner use's form is not resolved as a generated record's.
nestedSelfUses :: Map.Map TVar GenDef -> Set.Set TVar -> [RecordUse] -> [(TVar, [ArgF])]
nestedSelfUses defs recursive uses =
  unique
    [ (gv, ps)
    | (gv, _, ps, _) <- uses
    , Set.member gv recursive
    , Just gd <- [Map.lookup gv defs]
    , not (null (nativeParams gd))
    , any (mentions gv) (tyArgs ps)
    ]
  where
    mentions g t = case t of
      NamF _ (FV g' _) _ _ | g' == g -> True
      RecF (FV g' _) _ | g' == g -> True
      VarF (FV g' _) | g' == g -> True
      AppF (VarF (FV g' _)) _ | g' == g -> True
      _ -> any (mentions g) (childrenOf t)

-- | A use's arguments, in the definition's parameter order.
templateArgs :: Map.Map TVar GenDef -> GenDef -> [(Key, TypeF)] -> Either T.Text [TypeF]
templateArgs defs gd rs = do
  args <- inferArgsPartial defs gd rs
  mapM (\p -> maybe (Left ("the parameter " <> unTVar p <> " is not determined")) Right (Map.lookup p args)) (nativeParams gd)


-- | The serial forms of the values a pool marshals that no manifold's
-- arguments or result give: those its intrinsics marshal and the generated
-- records it holds, so that every instance the pool writes is known
-- (GEN-14). A value with no serial form is one no intrinsic can marshal
-- either, and the intrinsic reports it.
inPoolSerialASTs :: Lang -> [SerialManifold] -> MorlocMonad [SerialAST]
inPoolSerialASTs lang es = concat <$> mapM one (unique (concatMap intrinsicValueTypes es <> held))
  where
    -- A generated record the pool holds is marshalled wherever a value
    -- holding it is, the arms of a `data` type it is on a cycle with
    -- included, whether or not a value of it is written on its own.
    held = concatMap outermostGenerated (concatMap poolNativeTypes es)
    outermostGenerated t = case t of
      NamF _ (FV _ (CV "struct")) _ _ -> [t]
      _ -> concatMap outermostGenerated (childrenOf t)
    -- A type that has no serial form adds nothing; the state its attempt
    -- left is discarded. A held value holding a closure is written only
    -- where it crosses, with the closure wired there, so it adds nothing
    -- either.
    one t = do
      st0 <- MM.get
      MM.recoverRejected ((\a -> [a | null (serialClosuresOf a)]) <$> makeSerialAST 0 lang t) (MM.put st0 >> return [])

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
      RecF (FV g _) ps -> Set.member g pub && all (safe pub) (tyArgs ps)
      VariantF (FV g (CV f)) ps _ -> nominal pub g f ps
      EnumF (FV g (CV f)) ps _ -> nominal pub g f ps
      VarF (FV g (CV f)) -> nominal pub g f []
      AppF (VarF (FV _ (CV f))) as -> form f && all (safe pub) (tyArgs as)
      AppF h as -> safe pub h && all (safe pub) (tyArgs as)
      OptionalF x -> safe pub x
      FunF as r -> all (safe pub) (r : as)
      UnkF _ -> False
