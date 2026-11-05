{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Frontend.Instances
Description : The instance table and the rule that selects an instance
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

One entry per instance ('InstanceDecl', built by Link), and 'resolve', which
meets a class constraint at a use: by its alias chain, by the most specific
head that instantiates to its class arguments (CLS-16, CLS-17), and by the
selected instance's context (CLS-5).
-}
module Morloc.Frontend.Instances
  ( InstanceKey (..)
  , Impl (..)
  , InstanceDecl (..)
  , ClassInfo (..)
  , InstanceEnv (..)
  , Evidence (..)
  , ResolveError (..)
  , MissingMethod (..)
  , ResolvePolicy (..)
  , OpenClassArgs (..)
  , OmittedMethods (..)
  , defaultPolicy
  , packableWireFundep
  , mkInstanceDecl
  , insertInstanceDecl
  , insertClassInfo
  , instanceDecls
  , resolve
  , methodImpls
  , superclassObligations
  , fixOpenVars
  , couldMatch
  , fixRigidVars
  , explainResolveError
  , keyConstraint
  , keyNames
  , renameConstraint
  ) where

import qualified Data.List as DL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as DT
import Morloc.Data.Doc
import Morloc.Data.Zip (indexed, zipExact, zipPrefix)
import Morloc.Namespace.Expr
import Morloc.Namespace.Prim
import Morloc.Namespace.Type
import qualified Morloc.TypeEval as TE

data ResolveError
  = -- | No head matches; every instance of the class, for the message.
    NoInstance Constraint [InstanceKey]
  | -- | Several most specific heads match (CLS-16).
    Ambiguous Constraint [InstanceKey]
  | -- | The class arguments hold variables nothing at the use fixed (CLS-18).
    Undetermined Constraint [TVar]
  | -- | The selected instance's context fails (CLS-5).
    ContextUnmet InstanceKey Constraint ResolveError
  | -- | Context resolution repeats a constraint or grows without end; the
    -- chain from the use.
    ContextLoop [Constraint]
  | -- | DERIV-6 derives no comparison of a function or a suspension.
    NotCrossable Constraint
  | NotAClass Constraint
  deriving (Show, Eq, Ord)

data MissingMethod
  = MethodOmitted InstanceKey EVar
  | -- | Evidence that is not an instance has no implementations here.
    NotAnInstance Evidence
  deriving (Show, Eq, Ord)

-- | The choices open rulings leave to the caller. 'defaultPolicy' takes the
-- conservative one of each.
data ResolvePolicy = ResolvePolicy
  { policyOpenArgs :: OpenClassArgs
  , policyDerive :: Bool
  -- ^ DERIV-6: derive Eq and Ord where no instance matches
  , policyOmitted :: OmittedMethods
  }
  deriving (Show, Eq, Ord)

-- | Ruling Q1: a class argument holding a variable nothing fixed.
data OpenClassArgs
  = -- | Always 'Undetermined' (Q1 (a)).
    OpenArgsUndetermined
  | -- | Accepted when one head matches with the variable rigid and every
    -- head that unifies with the use generalizes it (Q1 (b)).
    OpenArgsSkolemUnique
  deriving (Show, Eq, Ord)

-- | CLS-12: a method the selected instance omits.
data OmittedMethods
  = -- | Taken from the most specific more general instance that gives it
    -- (answer (b), today's behaviour).
    OmittedFallThrough
  | -- | Missing (answer (a)).
    OmittedMissing
  deriving (Show, Eq, Ord)

defaultPolicy :: ResolvePolicy
defaultPolicy = ResolvePolicy OpenArgsUndetermined False OmittedFallThrough

-- | Ruling Q2 (a): Packable's wire argument is fixed by its native one.
-- Not installed in the table unless the ruling grants it.
packableWireFundep :: Map.Map ClassName [Int]
packableWireFundep = Map.singleton (ClassName "Packable") [0]

-- | An instance as one declaration writes it: the context is renamed with
-- the head, and a context variable the head lacks becomes an unsolved
-- existential, which no use can fix.
mkInstanceDecl :: InstanceOrigin -> ClassName -> [Constraint] -> [TypeU] -> Map.Map EVar [Impl] -> Int -> InstanceDecl
mkInstanceDecl origin cls ctx ts methods site =
  InstanceDecl
    { idKey = InstanceKey cls (vs, hs)
    , idContext = map (mapConstraint (openRest . renameVarsU table)) ctx
    , idMethods = methods
    , idSites = [site]
    , idOrigin = origin
    }
  where
    (vs, hs, table) = canonicalHeadWith ts
    keyVars = Set.fromList vs
    openRest t = DL.foldl' openVar t [v | (v, _) <- varForms t, not (Set.member v keyVars), isVarName v]
    openVar t v = substituteTVar v (ExistU (TV ("#ctx." <> unTVar v)) ([], Open) ([], Open)) t
    isVarName (TV n) = isTypeVariableName n

-- | Add a declaration, merging it into an existing entry of its key (CLS-2).
insertInstanceDecl :: InstanceDecl -> InstanceEnv -> InstanceEnv
insertInstanceDecl d env = env {ieByClass = Map.alter (Just . maybe [d] add) (ikClass (idKey d)) (ieByClass env)}
  where
    add ds = case DL.partition ((== idKey d) . idKey) ds of
      ([], _) -> ds <> [d]
      (old : _, _) -> [if idKey x == idKey d then mergeInto old else x | x <- ds]
    mergeInto old =
      old
        { idContext = DL.nub (idContext old <> idContext d)
        , idMethods = Map.unionWith (\a b -> DL.nub (a <> b)) (idMethods old) (idMethods d)
        , idSites = DL.nub (idSites old <> idSites d)
        }

insertClassInfo :: ClassName -> ClassInfo -> InstanceEnv -> InstanceEnv
insertClassInfo cls ci env = env {ieClasses = Map.insert cls ci (ieClasses env)}

instanceDecls :: InstanceEnv -> ClassName -> [InstanceDecl]
instanceDecls env cls = Map.findWithDefault [] cls (ieByClass env)

-- | How a constraint at a use is met, given the constraints of the signature
-- being checked.
--
-- A head matches a class argument tuple when it instantiates to it, at some
-- link of the tuple's alias chain; the nearest link with a match decides.
-- No coercion is applied (CLS-17): neither @?T@ nor an effect is stripped,
-- and effect rows match only when equal, a row variable binding nothing.
-- A record is matched by the name it was declared under. Language plays no
-- part. Positions in 'ieFundeps' do not select; a head is consistent with
-- such an argument when the argument is open or the head matches it.
resolve :: ResolvePolicy -> Scope -> InstanceEnv -> [Constraint] -> Constraint -> Either ResolveError Evidence
resolve policy scope env givens = go []
  where
    closure = superClosure env givens
    recordNames = declaredRecordNames scope

    go stack c@(Constraint cls args)
      | any (sameConstraint c) stack || length stack >= contextDepth = Left (ContextLoop (reverse (c : stack)))
      | otherwise = do
          let fundeps = Map.findWithDefault [] cls (ieFundeps env)
              selecting = [a | (i, a) <- indexed args, i `notElem` fundeps]
              open = DL.nub (concatMap openVars selecting)
          case () of
            _ | not (null open) && policyOpenArgs policy == OpenArgsUndetermined -> Left (Undetermined c open)
              | any (sameConstraint c) closure -> Right (EvGiven c)
              | otherwise -> select stack c fundeps open
    go _ c = Left (NotAClass c)

    select stack c@(Constraint cls args) fundeps open =
      case chainMatches fundeps decls args of
        Nothing
          | null open -> noInstance c
          | otherwise -> Left (Undetermined c open)
        Just (linkArgs, ms) -> case mostSpecificDecls fundeps ms of
          [(d, sub)]
            | not (null open) && not (skolemUnique fundeps d linkArgs) -> Left (Undetermined c open)
            | otherwise -> do
                let (vs, hs) = ikHead (idKey d)
                    tys = [Map.findWithDefault (keyVar hs v) v sub | v <- vs]
                ctxEv <- mapM (context stack c d sub) (idContext d)
                Right (EvInstance (idKey d) tys ctxEv)
          ds -> Left (Ambiguous c (map (idKey . fst) ds))
      where
        decls = instanceDecls env cls
    select _ c _ _ = Left (NotAClass c)

    context stack c d sub k =
      let k' = mapConstraint (substituteAll sub) k
       in case go (c : stack) k' of
            Left e@(ContextLoop _) -> Left e
            Left e -> Left (ContextUnmet (idKey d) k' e)
            Right ev -> Right ev

    noInstance c@(Constraint cls [t])
      | policyDerive policy
      , Just _ <- derivedComparisons (unClassName cls)
      , not (isRigidVar t) =
          if crossable t then Right (EvDerived cls t) else Left (NotCrossable c)
    noInstance c@(Constraint cls _) = Left (NoInstance c (map idKey (instanceDecls env cls)))
    noInstance c = Left (NotAClass c)

    -- The nearest link of the argument tuple's alias chain at which some
    -- head matches, with each matching declaration and its substitution.
    chainMatches fundeps decls args0 = walk fuel (tuple args0)
      where
        fuel = Map.size scope + depth (tuple args0) + maximum (0 : [depth (tuple hs) | d <- decls, let (_, hs) = ikHead (idKey d)]) + 1
        walk n t =
          case [(d, s) | d <- decls, Just s <- [matchDecl fundeps d (untuple t)]] of
            [] | n <= 0 -> Nothing
            [] -> case TE.reduceType scope t of
              Just t' | t' /= t -> walk (n - 1) t'
              _ -> case TE.reduceTypeLeaves scope t of
                Just t' | t' /= t -> walk (n - 1) t'
                _ -> Nothing
            ms -> Just (untuple t, ms)

    matchDecl fundeps d useArgs =
      let (vs, hs) = ikHead (idKey d)
          useNom = map (nominal recordNames) useArgs
          openFundep = or [not (null (openVars a)) | (i, a) <- indexed useNom, i `elem` fundeps]
          positions = if openFundep then [i | (i, _) <- indexed hs, i `notElem` fundeps] else map fst (indexed hs)
          pick xs = [x | (i, x) <- indexed xs, i `elem` positions]
       in if length hs /= length useNom
            then Nothing
            else matchGeneral True (quantify vs (tuple (pick hs))) (tuple (pick useNom))

    mostSpecificDecls fundeps ms =
      let pat d = selectionPattern fundeps (idKey d)
          heads = mostSpecific (map (pat . fst) ms)
       in [m | m@(d, _) <- ms, any (alphaEq (pat d)) heads]

    -- Q1 (b): every head that unifies with the use, its open variables
    -- flexible, generalizes the selected one.
    skolemUnique fundeps d useArgs
      | any hasPartialExist useArgs = False
      | otherwise =
          let selected = selectionPattern fundeps (idKey d)
              useFlex = map (nominal recordNames . flexExist) useArgs
              openNames = concatMap openVars useArgs
              others = [x | x <- instanceDecls env (ikClass (idKey d)), idKey x /= idKey d]
              unifies x =
                let (vs, hs) = ikHead (idKey x)
                 in length hs == length useFlex && isJust (unifyU (vs <> openNames) (tuple hs) (tuple useFlex))
           in and [generalizes True (selectionPattern fundeps (idKey x)) selected | x <- others, unifies x]

contextDepth :: Int
contextDepth = 64

-- | The head positions that select, quantified, for specificity.
selectionPattern :: [Int] -> InstanceKey -> TypeU
selectionPattern fundeps (InstanceKey _ (vs, hs)) =
  quantify vs (tuple [h | (i, h) <- indexed hs, i `notElem` fundeps])

quantify :: [TVar] -> TypeU -> TypeU
quantify vs t = foldr ForallU t vs

tuple :: [TypeU] -> TypeU
tuple = AppU (VarU (TV "__instance_head__"))

untuple :: TypeU -> [TypeU]
untuple (AppU _ xs) = xs
untuple t = [t]

-- | A key variable the use left unbound (it occurs only at a determined
-- position), in the carrier it has in the head.
keyVar :: [TypeU] -> TVar -> TypeU
keyVar hs v = case lookup v (concatMap varForms hs) of
  Just form -> rebuildVar form v
  Nothing -> VarU v

-- | Every variable of the map replaced at once: a value may name a key.
substituteAll :: Map.Map TVar TypeU -> TypeU -> TypeU
substituteAll sub t =
  Map.foldrWithKey substituteTVar (renameVarsU (Map.mapWithKey (\v _ -> placeholder v) sub) t) (Map.mapKeys placeholder sub)
  where
    placeholder (TV n) = TV ("#subst." <> n)

mapConstraint :: (TypeU -> TypeU) -> Constraint -> Constraint
mapConstraint f (Constraint c ts) = Constraint c (map f ts)
mapConstraint f (CMember a b) = CMember (f a) (f b)
mapConstraint f (CSubset a b) = CSubset (f a) (f b)
mapConstraint f (CDisjoint a b) = CDisjoint (f a) (f b)

sameConstraint :: Constraint -> Constraint -> Bool
sameConstraint (Constraint c1 ts1) (Constraint c2 ts2) =
  c1 == c2 && maybe False (all (uncurry alphaEq)) (zipExact ts1 ts2)
sameConstraint _ _ = False

-- | Constraints with every superclass constraint they imply.
superClosure :: InstanceEnv -> [Constraint] -> [Constraint]
superClosure env = go contextDepth []
  where
    go :: Int -> [Constraint] -> [Constraint] -> [Constraint]
    go _ seen [] = seen
    go n seen (c : cs)
      | any (sameConstraint c) seen = go n seen cs
      | n <= 0 = go n (seen <> [c]) cs
      | otherwise = go (n - 1) (seen <> [c]) (cs <> supers c)
    supers (Constraint cls args) = case Map.lookup cls (ieClasses env) of
      Just ci | Just pairs <- zipExact (ciVars ci) args ->
        map (mapConstraint (substituteAll (Map.fromList pairs))) (ciSupers ci)
      _ -> []
    supers _ = []

-- | The variables of a type that nothing has fixed: existentials and the
-- checker's generated variables.
openVars :: TypeU -> [TVar]
openVars t = here <> concatMap openVars (childrenU t)
  where
    here = case t of
      ExistU v _ _ -> [v]
      VarU v | generated v -> [v]
      KVarU (v, _) | generated v -> [v]
      _ -> []

generated :: TVar -> Bool
generated (TV n) = "@q" `DT.isInfixOf` n

-- | The constraints a declaration owes the superclasses of its class, at
-- its head (CLS-5).
superclassObligations :: InstanceEnv -> InstanceDecl -> [Constraint]
superclassObligations env d =
  let InstanceKey cls (_, hs) = idKey d
   in case Map.lookup cls (ieClasses env) of
        Just ci | Just pairs <- zipExact (ciVars ci) hs -> map (mapConstraint (substituteAll (Map.fromList pairs))) (ciSupers ci)
        _ -> []

-- | A constraint with each variable nothing fixed made a fixed one, and the
-- fixed names. An existential carrying part of a tuple or record stays
-- open.
fixOpenVars :: Constraint -> (Constraint, [TVar])
fixOpenVars c = (mapConstraint fix c, DL.nub (concatMap fixedIn (constraintTypes c)))
  where
    fix t = maybe (mapChildrenU fix t) id (fixable t)
    fixedIn t = maybe (concatMap fixedIn (childrenU t)) (map fst . varForms) (fixable t)
    fixable t = case t of
      ExistU v ([], _) ([], _) -> Just (VarU (fixedName v))
      VarU v | generated v -> Just (VarU (fixedName v))
      KVarU (v, k) | generated v -> Just (KVarU (fixedName v, k))
      _ -> Nothing
    fixedName (TV n) = TV ("?" <> DT.replace "@q" "" n)

-- | Whether some instance head of the constraint's class unifies with it,
-- at some link of its alias chain, once its open variables are solved. A
-- constraint holding an existential with partial fields may yet match.
couldMatch :: Scope -> InstanceEnv -> Constraint -> Bool
couldMatch scope env (Constraint cls args0)
  | any hasPartialExist args0 = True
  | otherwise = walk fuel (tuple (map (nominal recordNames . flexExist) args0))
  where
    recordNames = declaredRecordNames scope
    decls = instanceDecls env cls
    open = concatMap openVars args0
    fuel = Map.size scope + depth (tuple args0) + 1
    unifies t d =
      let (vs, hs) = ikHead (idKey d)
       in isJust (unifyU (vs <> open) (tuple hs) t)
    walk n t
      | any (unifies t) decls = True
      | n <= 0 = False
      | otherwise = case TE.reduceType scope t of
          Just t' | t' /= t -> walk (n - 1) t'
          _ -> case TE.reduceTypeLeaves scope t of
            Just t' | t' /= t -> walk (n - 1) t'
            _ -> False
couldMatch _ _ _ = True

-- | A constraint whose generated variables in @rigid@ (a signature's,
-- renamed by the checker) no longer read as open.
fixRigidVars :: Set.Set TVar -> Constraint -> Constraint
fixRigidVars rigid = mapConstraint fix
  where
    fix t = case t of
      VarU v | Set.member v rigid -> VarU (rigidName v)
      KVarU (v, k) | Set.member v rigid -> KVarU (rigidName v, k)
      _ -> mapChildrenU fix t
    rigidName (TV n) = TV (DT.replace "@q" "@" n)

constraintTypes :: Constraint -> [TypeU]
constraintTypes (Constraint _ ts) = ts
constraintTypes (CMember a b) = [a, b]
constraintTypes (CSubset a b) = [a, b]
constraintTypes (CDisjoint a b) = [a, b]

-- | An instance as a constraint, its variables named for reading.
keyConstraint :: InstanceKey -> Constraint
keyConstraint k@(InstanceKey cls (_, hs)) = renameConstraint (keyNames k) (Constraint cls hs)

-- | Readable names for an instance's variables.
keyNames :: InstanceKey -> Map.Map TVar TVar
keyNames (InstanceKey _ (vs, hs)) = Map.fromList (zipPrefix vs fresh)
  where
    taken = Set.fromList [v | (v, _) <- concatMap varForms hs, v `notElem` vs]
    fresh = [v | k <- [0 :: Int ..], let v = TV (letter k), not (Set.member v taken)]
    letter k = if k < 26 then DT.singleton (toEnum (fromEnum 'a' + k)) else "t" <> DT.pack (show k)

renameConstraint :: Map.Map TVar TVar -> Constraint -> Constraint
renameConstraint names = mapConstraint (renameVarsU names)

-- | Why a constraint is not met, for a message; @pc@ shows a constraint.
explainResolveError :: (Constraint -> MDoc) -> ResolveError -> MDoc
explainResolveError pc e = case e of
  NoInstance c _ -> "no instance gives" <+> pc c
  Ambiguous c ks ->
    "several instances match" <+> pc c <+> "and none is more specific than the others ("
      <> hsep (punctuate "," (map (pc . keyConstraint) ks)) <> ")"
  Undetermined c vs ->
    "nothing fixes" <+> hsep (punctuate "," (map pretty vs)) <+> "in" <+> pc c
  ContextUnmet k c e' ->
    "the instance" <+> pc (keyConstraint k) <+> "needs" <+> pc c <> ", and" <+> explainResolveError pc e'
  ContextLoop cs ->
    "resolving it does not end:" <+> hsep (punctuate " needs" (map pc cs))
  NotCrossable c -> pc c <+> "is not derived for a function or a suspension"
  NotAClass c -> pc c <+> "is not a class constraint"

-- | An existential carrying part of a tuple or record: it is not one type a
-- variable can stand for.
hasPartialExist :: TypeU -> Bool
hasPartialExist t = case t of
  ExistU _ (ps, _) (rs, _) | not (null ps && null rs) -> True
  _ -> any hasPartialExist (childrenU t)

flexExist :: TypeU -> TypeU
flexExist t = case t of
  ExistU v ([], _) ([], _) -> VarU v
  _ -> mapChildrenU flexExist t

isRigidVar :: TypeU -> Bool
isRigidVar (VarU (TV n)) = isTypeVariableName n
isRigidVar _ = False

crossable :: TypeU -> Bool
crossable t = case t of
  FunU _ _ -> False
  EffectU _ _ -> False
  _ -> all crossable (childrenU t)

-- | A record's constructor name mapped to the name its declaration gives
-- it, where the two differ (@record Foo = Bar {..}@) and the constructor
-- belongs to one declaration.
declaredRecordNames :: Scope -> Map.Map TVar TVar
declaredRecordNames scope =
  Map.mapMaybe one $
    Map.fromListWith (<>) [(n, [k]) | (k, defs) <- Map.toList scope, (_, NamU _ n _ _, _, _, _) <- defs, n /= k]
  where
    one ks = case DL.nub ks of
      [k] -> Just k
      _ -> Nothing

-- | An evaluated record named as its head would name it.
nominal :: Map.Map TVar TVar -> TypeU -> TypeU
nominal names t = case t of
  NamU _ n ps _ ->
    let n' = Map.findWithDefault n n names
     in if null ps then VarU n' else AppU (VarU n') (map (nominal names) ps)
  _ -> mapChildrenU (nominal names) t

depth :: TypeU -> Int
depth t = 1 + maximum (0 : map depth (childrenU t))

childrenU :: TypeU -> [TypeU]
childrenU t = case t of
  ExistU _ (ps, _) (rs, _) -> ps <> map snd rs
  ForallU _ x -> [x]
  FunU ts r -> ts <> [r]
  AppU h ts -> h : ts
  NamU _ _ ps rs -> ps <> map snd rs
  EffectU _ x -> [x]
  OptionalU x -> [x]
  OpU _ ts -> ts
  LitU (LRec fs) -> map snd fs
  LitU (LList es) -> es
  LitU (LSet es) -> es
  LabeledU _ x -> [x]
  _ -> []

mapChildrenU :: (TypeU -> TypeU) -> TypeU -> TypeU
mapChildrenU f t = case t of
  ExistU v (ps, pc) (rs, rc) -> ExistU v (map f ps, pc) ([(k, f x) | (k, x) <- rs], rc)
  ForallU v x -> ForallU v (f x)
  FunU ts r -> FunU (map f ts) (f r)
  AppU h ts -> AppU (f h) (map f ts)
  NamU o n ps rs -> NamU o n (map f ps) [(k, f x) | (k, x) <- rs]
  EffectU es x -> EffectU es (f x)
  OptionalU x -> OptionalU (f x)
  OpU op ts -> OpU op (map f ts)
  LitU (LRec fs) -> LitU (LRec [(k, f x) | (k, x) <- fs])
  LitU (LList es) -> LitU (LList (map f es))
  LitU (LSet es) -> LitU (LSet (map f es))
  LabeledU v x -> LabeledU v (f x)
  _ -> t

-- | The implementations a method has under the evidence: those the
-- selected instance gives, or under 'OmittedFallThrough' those of the most
-- specific more general instance that gives some, plus the representation
-- casts of the more general instances (NEWT-7). CLS-8 defaults join here
-- once CLS-7 parses.
methodImpls :: ResolvePolicy -> InstanceEnv -> Evidence -> EVar -> Either MissingMethod [Impl]
methodImpls policy env ev@(EvInstance key tys _) m =
  case ([d | d <- instanceDecls env (ikClass key), idKey d == key], zipExact (fst (ikHead key)) tys) of
    ([], _) -> Left (NotAnInstance ev)
    (_, Nothing) -> Left (NotAnInstance ev)
    (d : _, Just inst) ->
      let impls x = Map.findWithDefault [] m (idMethods x)
          isCast (ImplCast _) = True
          isCast _ = False
          hs = snd (ikHead key)
          selected = tuple (map (substituteAll (Map.fromList inst)) hs)
          general =
            [ x
            | x <- instanceDecls env (ikClass key)
            , idKey x /= key
            , let (xvs, xhs) = ikHead (idKey x)
            , generalizes True (quantify xvs (tuple xhs)) selected
            ]
          given = impls d
          fallback = case policyOmitted policy of
            OmittedMissing -> []
            OmittedFallThrough ->
              let giving = [x | x <- general, any (not . isCast) (impls x)]
                  pat x = let (xvs, xhs) = ikHead (idKey x) in quantify xvs (tuple xhs)
                  heads = mostSpecific (map pat giving)
               in case [x | x <- giving, any (alphaEq (pat x)) heads] of
                    [x] -> impls x
                    _ -> []
          base = if null given then fallback else given
          result = base <> [i | x <- general, i <- impls x, isCast i, i `notElem` base]
       in if null result then Left (MethodOmitted key m) else Right result
methodImpls _ _ ev _ = Left (NotAnInstance ev)
