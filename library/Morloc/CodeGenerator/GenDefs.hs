{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.GenDefs
Description : Generated type definitions read from their declarations (GEN-9)

A generated record is defined once per language from its declaration, not
from its uses. The definition is the record applied to its own parameters,
each parameter standing for itself: a placeholder type whose native form
is the parameter's native name. A use is then an instance of the
definition at the arguments it carries.
-}
module Morloc.CodeGenerator.GenDefs
  ( GenDef (..)
  , genDefs
  , DataDef (..)
  , dataDefs
  , instantiate
  , checkUse
  , Disagreement (..)
  , untiedSelf
  , RecordUse
  , recordUses
  , disagreements
  , inPoolSerialASTs
  , placeTypes
  , carriedTemplateArgs
  , nestedSelfUses
  , nestedSelfUseReason
  , placeholders
  , nativeParam
  , nativeParams
  , nativeDataParams
  , armDataParams
  , dataNamesIn
  ) where

import qualified Data.Text as T
import Morloc.CodeGenerator.Infer (concreteNameOf, getScope, heldDataParams, inferConcreteType)
import Morloc.CodeGenerator.Namespace
import qualified Data.Set as Set
import Morloc.CodeGenerator.Grammars.Common (intrinsicValueTypes, poolNativeTypes, recordsInType, serialClosuresOf)
import Morloc.CodeGenerator.Grammars.Macro (expandMacro)
import Morloc.CodeGenerator.Serial (makeSerialAST)
import Morloc.TypeEval (recordNestingLimit)
import qualified Morloc.Data.Map as Map
import Morloc.Data.Zip (indexedFrom, zipExact, zipSame)
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
      r <- (Right <$> atPlaceholders lang gv ps) `catchRejected` (return . Left)
      case r of
        Right t@(NamF _ _ _ rs) -> case untiedSelf t of
          Just (g, True) -> MM.throwCompilerBug ("a reference of the record " <> pretty g <> " to itself was not tied in inference")
          -- A reference to the record at other arguments is another
          -- instance of it, which is not generated yet.
          Just (g, False) ->
            return (gv, Left ("it refers to " <> unTVar g <> " at other arguments than its own, which is not generated yet"))
          Nothing -> return (gv, Right (GenDef gv form ps rs))
        Right _ -> return (gv, Left "its declaration does not resolve to a record")
        Left e -> return (gv, Left e)

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
    held = Set.fromList (concatMap (placeholders (== ddVar dd)) (concat [ts | (c', ts) <- ddArms dd, c' == c]))

-- | The `data` types named anywhere in the types.
dataNamesIn :: [TypeF] -> Set.Set TVar
dataNamesIn = Set.fromList . concatMap go
  where
    go t = case t of
      VariantF (FV g _) _ _ -> g : concatMap go (childrenOf t)
      RecF (FV g _) _ -> g : concatMap go (childrenOf t)
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

-- | A generated record referred to by name inside an occurrence of itself,
-- with whether the reference is at the occurrence's own arguments: one
-- there inference should have tied ('RecF'); one at other arguments names
-- another instance. A record mapped to a native form is named through it,
-- at any arguments.
untiedSelf :: TypeF -> Maybe (TVar, Bool)
untiedSelf t0 = let hits = go Map.empty t0 in find snd hits <|> listToMaybe hits
  where
    go anc t = case t of
      NamF _ (FV g (CV "struct")) ps rs -> concatMap (go (Map.insert g ps anc) . snd) rs
      VarF (FV g _) | Just ps <- Map.lookup g anc -> [(g, null ps)]
      AppF (VarF (FV g _)) as | Just ps <- Map.lookup g anc -> [(g, as == ps)]
      _ -> concatMap (go anc) (childrenOf t)

-- | Replace each placeholder by its argument.
instantiate :: Map.Map TVar TypeF -> TypeF -> TypeF
instantiate args t
  | Just p <- paramOf t = Map.findWithDefault t p args
  | otherwise = mapChildren (instantiate args) t

-- | Whether a use is an instance of its record's definition at the
-- arguments it carries: Nothing, or the first disagreement. Field types are
-- compared by key, which names a declared type by its arguments without its
-- fields, so a recursive record unrolled to any depth, or tied, is one type.
checkUse :: GenDef -> [ArgF] -> [(Key, TypeF)] -> Maybe Disagreement
checkUse gd carried rs = case zipExact [p | (p, KindType) <- gdParams gd] (tyArgs carried) of
  Nothing -> Just (Unmatched "it carries another number of type arguments than the record has parameters")
  Just args
    | map fst rs /= map fst (gdFields gd) -> Just (Unmatched "its fields are not the declaration's, in the declaration's order")
    | otherwise ->
        firstJust id
          [ if typeFKey d' == typeFKey u then Nothing else Just (FieldDiffers k d' u)
          | ((k, d), (_, u)) <- zipSame (gdFields gd) rs
          , let d' = instantiate (Map.fromList args) d
          ]

-- | Why a use is not an instance of its record's definition.
data Disagreement
  = Unmatched T.Text
  -- ^ its arguments or fields are not the declaration's
  | FieldDiffers Key TypeF TypeF
  -- ^ a field, as the definition has it at the use's arguments, and as
  -- the use has it
  deriving (Show, Eq)

-- | The parameters a type holds in a field, the ones its uses can bind:
-- the arguments a nested record or `data` type carries are not counted,
-- since its fields say what it holds.
placeholders :: (TVar -> Bool) -> TypeF -> [TVar]
placeholders _ t
  | Just p <- paramOf t = [p]
placeholders isData t = case t of
  NamF _ _ _ rs -> concatMap (placeholders isData . snd) rs
  FunF as r -> concatMap (placeholders isData) (r : as)
  -- A native form that does not spell its arguments holds none of them.
  AppF h@(VarF (FV _ (CV form))) _
    | not (T.any (== '$') form) -> placeholders isData h
  AppF h as -> placeholders isData h <> concatMap (placeholders isData) (tyArgs as)
  OptionalF x -> placeholders isData x
  -- A `data` template spells the arguments at its `$k` slots, in an
  -- occurrence and in a reference alike.
  VariantF fv ps _ | Just xs <- spelled fv ps -> xs
  RecF fv@(FV g _) ps | isData g, Just xs <- spelled fv ps -> xs
  VariantF _ _ as -> concatMap (concatMap (placeholders isData) . snd) as
  _ -> []
  where
    spelled (FV _ (CV form)) ps
      | T.any (== '$') form = Just (concatMap (placeholders isData) [x | (k, x) <- indexedFrom 1 (tyArgs ps), k `elem` macroSlots form])
      | otherwise = Nothing

-- | The positions a native form's `$k` slots name.
macroSlots :: T.Text -> [Int]
macroSlots form = [k | piece <- drop 1 (T.splitOn "$" form), Just k <- [readMaybe (T.unpack (T.takeWhile isDigit piece))]]

-- | The parameters a record's native form takes (GEN-9): its type
-- parameters some field holds, in order. Any other parameter is erased.
nativeParams :: GenDef -> [TVar]
nativeParams gd = [p | (p, KindType) <- gdParams gd, Set.member p held]
  where
    held = Set.fromList (concatMap (placeholders (const False) . snd) (gdFields gd))

-- | The native name of a parameter (D14).
nativeParam :: TVar -> T.Text
nativeParam p = "T_" <> unTVar p

-- | An occurrence of a record: its name, its native form, the arguments
-- it carries and its fields.
type RecordUse = (TVar, T.Text, [ArgF], [(Key, TypeF)])

-- | The occurrences of records in the given types, at any depth.
recordUses :: [TypeF] -> [RecordUse]
recordUses ts = unique [(gv, cv, ps, rs) | (FV gv (CV cv), ps, rs) <- concatMap recordsInType ts]

-- | Each use of one of the given records, at its definition's form, that
-- is not an instance of its definition (GEN-9), with why.
disagreements :: Map.Map TVar GenDef -> [RecordUse] -> [(TVar, Disagreement)]
disagreements defs uses =
  [ (gv, d)
  | (gv, cv, ps, rs) <- uses
  , Just gd <- [Map.lookup gv defs]
  , gdForm gd == cv
  , Just d <- [checkUse gd ps rs]
  ]

-- | A reference's arguments at its record's native parameters: those it
-- carries, which on the native side are its own ('RecF' has no fields to
-- bind them).
carriedTemplateArgs :: GenDef -> [ArgF] -> Maybe [TypeF]
carriedTemplateArgs gd ps = do
  pairs <- zipExact [p | (p, KindType) <- gdParams gd] (tyArgs ps)
  return [t | (p, t) <- pairs, p `elem` nativeParams gd]

-- | The recursive parameterized records used at an argument that refers to
-- the record by name only, which are not generated: concrete inference cuts
-- a record's expansion by name where it is nested in recursive records more
-- deeply than 'recordNestingLimit' allows, or where it is non-regular (`Nest [a]` in `Nest
-- a`), so the inner use's form is not resolved as a generated record's.
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
      VarF (FV g' _) | g' == g -> True
      AppF (VarF (FV g' _)) _ | g' == g -> True
      _ -> any (mentions g) (childrenOf t)

-- | Why a use 'nestedSelfUses' finds is not generated, after the record's
-- name.
nestedSelfUseReason :: T.Text
nestedSelfUseReason =
  "refers to itself and is used where its expansion is cut by name: nested in recursive records more than "
    <> T.pack (show (recordNestingLimit + 1)) <> " levels deep, or at arguments that change with each nesting;"
    <> " this is not generated yet"

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
      RecF (FV g (CV f)) ps -> nominal pub g f ps
      VariantF (FV g (CV f)) ps _ -> nominal pub g f ps
      EnumF (FV g (CV f)) ps _ -> nominal pub g f ps
      VarF (FV g (CV f)) -> nominal pub g f []
      AppF (VarF (FV _ (CV f))) as -> form f && all (safe pub) (tyArgs as)
      AppF h as -> safe pub h && all (safe pub) (tyArgs as)
      OptionalF x -> safe pub x
      FunF as r -> all (safe pub) (r : as)
      UnkF _ -> False
