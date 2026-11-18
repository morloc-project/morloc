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
  , RecordUse
  , recordUses
  , disagreements
  , inPoolSerialASTs
  , placeTypes
  , carriedTemplateArgs
  , placeholders
  , paramOf
  , nativeParam
  , nativeParams
  , nativeDataParams
  , armDataParams
  , dataNamesIn
  ) where

import qualified Data.Text as T
import Morloc.CodeGenerator.Infer (concreteNameOf, getScope, heldParams, erasedType, inferConcreteType, macroSlots, spellingOf)
import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Nominal (KeyArg (..), KeyKind (..), KeyType (..), NomKey (..))
import qualified Data.Set as Set
import Morloc.CodeGenerator.Grammars.Common (intrinsicValueTypes, poolNativeTypes, recordsInType, serialClosuresOf)
import Morloc.CodeGenerator.Grammars.Macro (expandMacro)
import Morloc.CodeGenerator.Serial (makeSerialAST)
import qualified Morloc.Data.Map as Map
import Morloc.Data.Zip (indexedFrom, zipExact, zipSame)
import qualified Morloc.Monad as MM
import Control.Monad (unless)
import Control.Monad.Except (catchError)
import Morloc.Data.Doc (pretty, render, (<+>))

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
        Right (NamF _ _ _ rs) -> do
          let gd = GenDef gv form ps rs
          (cscope, gscope) <- getScope lang
          sp <- spellingOf lang
          unless (all (`elem` heldParams sp gscope cscope gv) (nativeParams gd)) $
            MM.throwCompilerBug ("the native parameters of" <+> pretty gv <+> "are not among those a value of it holds")
          return (gv, Right gd)
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
    -- A parameter of another kind has no native form (GEN-9): its erased
    -- value.
    paramArg (_, k) = KiA (VoidT k)

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
-- parameters some arm holds, in declaration order (Infer.heldParams).
nativeDataParams :: DataDef -> [TVar]
nativeDataParams = ddNative

-- | The native parameters one arm of a definition holds, in order: the
-- parameters of a language's per-arm type where one may not take more.
armDataParams :: DataDef -> T.Text -> [TVar]
armDataParams dd c = [p | p <- ddNative dd, Set.member p held]
  where
    held = Set.fromList (concatMap placeholders (concat [ts | (c', ts) <- ddArms dd, c' == c]))

-- | The `data` types named anywhere in the types, and the records named by
-- a reference ('RecF').
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
  sp <- spellingOf lang
  results <- mapM (one gscope cscope sp) [gv | gv <- Set.toList used, concreteNameOf cscope gv == unTVar gv, not (null (heldParams sp gscope cscope gv))]
  return (Map.fromList [(gv, dd) | Just (gv, Right dd) <- results], Map.fromList [(gv, e) | Just (gv, Left e) <- results])
  where
    one gscope cscope sp gv = case [ps | (ps0, _, _, _, TypedefEnum) <- Map.findWithDefault [] gv gscope, Just ps <- [mapM (either Just (const Nothing)) ps0]] of
      (ps : _) ->
        ( ( \tf -> case tf of
              VariantF (FV _ (CV form)) _ arms | T.any (== '$') form -> Just (gv, Right (DataDef gv form [p | (p, KindType) <- ps] (heldParams sp gscope cscope gv) arms))
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

-- | Replace each placeholder by its argument.
instantiate :: Map.Map TVar TypeF -> TypeF -> TypeF
instantiate args t
  | Just p <- paramOf t = Map.findWithDefault t p args
  | otherwise = mapChildren (instantiate args) t

-- | Whether a use is an instance of its record's definition at the
-- arguments it carries: Nothing, or the first disagreement. Field types are
-- compared by key ('fieldKey'), which names a declared type by its
-- arguments without its fields, so a recursive record unrolled to any
-- depth, or tied, is one type.
checkUse :: GenDef -> [ArgF] -> [(Key, TypeF)] -> Maybe Disagreement
checkUse gd carried rs = case zipExact [p | (p, KindType) <- gdParams gd] (tyArgs carried) of
  Nothing -> Just (Unmatched "it carries another number of type arguments than the record has parameters")
  Just args
    | map fst rs /= map fst (gdFields gd) -> Just (Unmatched "its fields are not the declaration's, in the declaration's order")
    | otherwise ->
        firstJust id
          [ if fieldKey d' == fieldKey u then Nothing else Just (FieldDiffers k d' u)
          | ((k, d), (_, u)) <- zipSame (gdFields gd) rs
          , let d' = instantiate (Map.fromList args) d
          ]

-- | A field type's key without its `Nat`, `Str` and erased kind arguments,
-- which have no native form (GEN-9) and which a use's field, read back from
-- its serial form, does not keep. A row stays: a table's form keeps it.
fieldKey :: TypeF -> KeyType
fieldKey = go . typeFKey
  where
    go k = case k of
      KFun as r -> KFun (map go as) (go r)
      KApp h as -> KApp (go h) (args as)
      KOpt x -> KOpt (go x)
      KNom (NomKey c as) -> KNom (NomKey c (args as))
      KAnon o rs -> KAnon o [(f, go x) | (f, x) <- rs]
      _ -> k
    args as = [a' | Just a' <- map arg as]
    arg (KTy t) = Just (KTy (go t))
    arg (KKind (KRow cols)) = Just (KKind (KRow [(c, go t) | (c, t) <- cols]))
    arg (KKind _) = Nothing

-- | Why a use is not an instance of its record's definition.
data Disagreement
  = Unmatched T.Text
  -- ^ its arguments or fields are not the declaration's
  | FieldDiffers Key TypeF TypeF
  -- ^ a field, as the definition has it at the use's arguments, and as
  -- the use has it
  deriving (Show, Eq)

-- | The parameters a type holds in a field, the ones its uses can bind:
-- the arguments a nested generated record or `data` type carries are not
-- counted, since its fields say what it holds; a native form spells the
-- arguments at its `$k` slots, in an occurrence and in a reference alike,
-- and a reference spelled there spells its own arguments.
placeholders :: TypeF -> [TVar]
placeholders = go False
  where
    go _ t | Just p <- paramOf t = [p]
    go slot t = case t of
      NamF _ fv ps _ | Just xs <- spelled fv ps -> xs
      NamF _ _ _ rs -> concatMap (go slot . snd) rs
      FunF as r -> concatMap (go slot) (r : as)
      -- A native form that does not spell its arguments holds none of them.
      AppF h@(VarF (FV _ (CV form))) _
        | not (T.any (== '$') form) -> go slot h
      AppF h as -> go slot h <> concatMap (go slot) (tyArgs as)
      OptionalF x -> go slot x
      VariantF fv ps _ | Just xs <- spelled fv ps -> xs
      RecF fv ps
        | Just xs <- spelled fv ps -> xs
        | slot -> concatMap (go slot) (tyArgs ps)
      VariantF _ _ as -> concatMap (concatMap (go slot) . snd) as
      _ -> []
    spelled (FV _ (CV form)) ps
      | T.any (== '$') form = Just (concatMap (go True) [x | (k, x) <- indexedFrom 1 (tyArgs ps), k `elem` macroSlots form])
      | otherwise = Nothing


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
    -- A parameter, and an erased argument, have no native form to import.
    safe _ t
      | isJust (paramOf t) || t == erasedType = True
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
