{-|
Module      : Morloc.TypeChecker.Namespace
Description : Data structures and related functions
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.TypeChecker.Namespace
(
    Stack
  , Expr(..)
  , EVar(..)
  , TVar(..)
  , MVar(..)
  , Type(..)
  , Gamma
  , GammaIndex(..)
  , Module(..)
  , Import(..)
  , Language
  , cut
  , (+>)
  , TypeError(..)
  , access1
  , access2
  , ann
  , lookupT
  , lookupE
  , lookupSrc
  , throwError
  , runStack
  , index
  , generalize
  , generalizeE
  , mapT
  , mapT'
  -- * State manipulation
  , newvar
  , newqul
  -- * pretty printing
  , prettyExpr
  , prettyModule
  , prettyType
  , desc
  , toType
  , fromType
  -- * ModuleGamma paraphernalia
  , ModularGamma
  , importFromModularGamma
  , extendModularGamma
  -- * Type extensions
  , EType(..)
  , TypeSet(..)
  , Property(..)
  , Constraint(..)
) where

import Morloc.Global (Path)
import Morloc.Operators
import qualified Data.List as DL
import Control.Monad.Except (throwError)
import qualified Control.Monad.Except as ME
import qualified Control.Monad.State as MS
import qualified Control.Monad.Writer as MW
import qualified Control.Monad.Reader as MR
import qualified Control.Monad.Identity as MI
import qualified Control.Monad as CM
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Text.Prettyprint.Doc.Render.Terminal.Internal
import qualified Data.Scientific as DS

type GeneralStack c e l s a = MR.ReaderT c (ME.ExceptT e (MW.WriterT l (MS.StateT s MI.Identity))) a
type Stack a = GeneralStack StackConfig TypeError [T.Text] StackState a

-- | currently I do nothing with the Reader and Writer monads, but I'm leaving
-- them in for now since I will need them when I plug this all into Morloc.
runStack :: Stack a -> (Either TypeError a, [T.Text])
runStack e
  = fst
  . MI.runIdentity
  . flip MS.runStateT emptyState
  . MW.runWriterT
  . ME.runExceptT
  . MR.runReaderT e
  $ StackConfig 0
         
type Gamma = [GammaIndex]
newtype EVar = EV T.Text deriving(Show, Eq, Ord)
newtype MVar = MV T.Text deriving(Show, Eq, Ord)
newtype TVar = TV T.Text deriving(Show, Eq, Ord)

data StackState = StackState {
      stateVar :: Int
    , stateQul :: Int
  } deriving(Ord, Eq, Show)
emptyState = StackState 0 0

data StackConfig = StackConfig {
      configVerbosity :: Int -- Not currently used
  }

-- | A context, see Dunfield Figure 6
data GammaIndex
  = VarG TVar
  -- ^ (G,a)
  | AnnG Expr TypeSet
  -- ^ (G,x:A) looked up in the (Var) and cut in (-->I)
  | ExistG TVar
  -- ^ (G,a^) unsolved existential variable
  | SolvedG TVar Type
  -- ^ (G,a^=t) Store a solved existential variable
  | MarkG TVar
  -- ^ (G,>a^) Store a type variable marker bound under a forall
  | MarkEG EVar
  -- ^ ...
  | SrcG (EVar, Language, Maybe Path, EVar)
  deriving(Ord, Eq, Show)

data Import = Import {
    importModuleName :: MVar
  , importInclude :: Maybe [(EVar, EVar)]
  , importExclude :: [EVar]
  , importNamespace :: Maybe EVar -- currently not used
} deriving (Ord, Eq, Show)

data Module = Module {
    moduleName :: MVar
  , modulePath :: Maybe Path
  , moduleImports :: [Import]
  , moduleExports :: [EVar]
  , moduleBody :: [Expr]
} deriving (Ord, Eq, Show)

-- | Terms, see Dunfield Figure 1
data Expr
  = SrcE Language (Maybe Path) [(EVar, EVar)]
  -- ^ import "c" from "foo.c" ("f" as yolo)
  | Signature EVar EType
  -- ^ x :: A; e
  | Declaration EVar Expr
  -- ^ x=e1; e2
  | UniE
  -- ^ (())
  | VarE EVar 
  -- ^ (x)
  | ListE [Expr]
  -- ^ [e]
  | TupleE [Expr]
  -- ^ (e1), (e1,e2), ... (e1,e2,...,en)
  | LamE EVar Expr
  -- ^ (\x -> e)
  | AppE Expr Expr
  -- ^ (e e)
  | AnnE Expr Type
  -- ^ (e : A)
  | NumE DS.Scientific | LogE Bool | StrE T.Text 
  -- ^ primitives
  | RecE [(EVar, Expr)]
  deriving(Show, Ord, Eq)

-- | Types, see Dunfield Figure 6
data Type
  = UniT
  -- ^ (1)
  | VarT TVar
  -- ^ (a)
  | ExistT TVar
  -- ^ (a^) will be solved into one of the other types
  | Forall TVar Type
  -- ^ (Forall a . A)
  | FunT Type Type
  -- ^ (A->B)
  | ArrT TVar [Type]
  -- ^ f [Type]
  | RecT [(TVar, Type)]
  -- ^ Foo { bar :: A, baz :: B }
  deriving(Show, Ord, Eq)

data Property
  = Pack   -- data structure to JSON
  | Unpack -- JSON to data structure
  | Cast   -- casts from type A to B
  | GeneralProperty [T.Text]
  deriving(Show, Eq, Ord)

-- | Eventually, Constraint should be a richer type, but for they are left as
-- unparsed lines of text
newtype Constraint = Con T.Text deriving(Show, Eq, Ord)

-- | Eventually Lang should be an enumeration with a term for each supported
-- language (as it is in Morloc). But until Xi is connected with Morloc, I will
-- leave it as general text.
type Language = T.Text

-- | Extended Type that may represent a language specific type as well as sets
-- of properties and constrains.
data EType = EType {
    etype :: Type
  , elang :: Maybe Language
  , eprop :: Set.Set Property
  , econs :: Set.Set Constraint
  , esource :: Maybe (Maybe Path, EVar)
} deriving(Show, Eq, Ord)

data TypeSet = TypeSet (Maybe EType) [EType] deriving(Show, Eq, Ord)

data TypeError
  = UnknownError
  | SubtypeError Type Type
  | ExistentialError
  | BadExistentialCast
  | AccessError T.Text
  | NonFunctionDerive
  | UnboundVariable EVar
  | OccursCheckFail
  | EmptyCut
  | TypeMismatch
  | UnexpectedPattern Expr Type
  | ToplevelRedefinition
  | NoAnnotationFound -- I don't know what this is for
  | NotImplemented -- this should only be used as a placeholder
  | OtherError T.Text
  -- container errors
  | EmptyTuple
  | TupleSingleton
  | EmptyRecord
  -- module errors
  | MultipleModuleDeclarations MVar
  | BadImport MVar EVar
  | CannotFindModule MVar
  | CyclicDependency
  | CannotImportMain
  | SelfImport MVar
  | BadRealization
  | MissingSource
  -- type extension errors
  | AmbiguousPacker TVar
  | AmbiguousUnpacker TVar
  | AmbiguousCast TVar TVar
  | IncompatibleRealization MVar
  | MissingAbstractType
  | ExpectedAbstractType
  deriving(Show, Ord, Eq)

type ModularGamma = Map.Map MVar (Map.Map EVar TypeSet)

class Typed a where
  toType :: a -> Maybe Type
  fromType :: Type -> a
  
instance Typed EType where
  toType e = case elang e of 
    (Just _) -> Nothing
    Nothing -> Just (etype e)

  fromType t = EType
    { etype = t
    , elang = Nothing
    , eprop = Set.empty
    , econs = Set.empty
    , esource = Nothing
    }  

instance Typed TypeSet where
  toType (TypeSet (Just e) _) = toType e
  toType (TypeSet Nothing _) = Nothing

  fromType t = TypeSet (Just (fromType t)) []

instance Typed Type where
  toType = Just
  fromType = id


importFromModularGamma :: ModularGamma -> Module -> Stack Gamma
importFromModularGamma g m = fmap concat $ mapM lookupImport (moduleImports m) where
  lookupOneImport :: MVar -> Map.Map EVar TypeSet -> (EVar, EVar) -> Stack GammaIndex 
  lookupOneImport v typemap (n, alias) = case Map.lookup n typemap of
    (Just t) -> return $ AnnG (VarE alias) t
    Nothing -> throwError $ BadImport v alias

  lookupImport :: Import -> Stack Gamma
  lookupImport imp
    | v == moduleName m = throwError $ SelfImport v
    | v == MV "Main" = throwError CannotImportMain
    | otherwise = case (importInclude imp, Map.lookup v g) of
        -- raise error if the imported module is not in the module map
        (_, Nothing) -> throwError $ CannotFindModule v
        -- handle imports of everything, i.e. @import Foo@
        (Nothing, Just g') -> return [AnnG (VarE e) t | (e,t) <- Map.toList g']
        -- handle limited imports, i.g. @import Foo ("f" as foo, bar)@
        (Just xs, Just g') -> mapM (lookupOneImport v g') xs
    where
      v = importModuleName imp

extendModularGamma
  :: Gamma -- ^ context generated from typechecking this module
  -> Module -- ^ the module that is being loaded into the modular context
  -> ModularGamma -- ^ the previous object
  -> Stack ModularGamma
extendModularGamma g m mg
  | Map.member v mg = throwError $ MultipleModuleDeclarations v
  | otherwise = return $ Map.insert v g' mg
  where
    v = moduleName m
    es = moduleExports m
    g' = Map.fromList [(e, t) | (AnnG (VarE e) t) <- g, elem e es]

mapT :: (Type -> Type) -> Expr -> Expr
mapT f (LamE v e) = LamE v (mapT f e)
mapT f (ListE es) = ListE (map (mapT f) es)
mapT f (TupleE es) = TupleE (map (mapT f) es)
mapT f (AppE e1 e2) = AppE (mapT f e1) (mapT f e2)
mapT f (AnnE e t) = AnnE (mapT f e) (f t)
mapT f (Declaration v e) = Declaration v (mapT f e)
mapT f (Signature v e) = Signature v $ e {etype = f (etype e)}
mapT _ e = e

mapT' :: Monad m => (Type -> m Type) -> Expr -> m Expr
mapT' f (LamE v e) = LamE <$> pure v <*> mapT' f e
mapT' f (ListE es) = ListE <$> mapM (mapT' f) es
mapT' f (TupleE es) = TupleE <$> mapM (mapT' f) es
mapT' f (AppE e1 e2) = AppE <$> mapT' f e1 <*> mapT' f e2
mapT' f (AnnE e t) = AnnE <$> mapT' f e <*> f t
mapT' f (Declaration v e) = Declaration <$> pure v <*> mapT' f e
mapT' f (Signature v e) = do
  t' <- f (etype e) 
  return $ Signature v (e {etype = t'})
mapT' _ e = return e

(+>) :: Indexable a => Gamma -> a -> Gamma
(+>) xs x = (index x):xs

-- | remove context up to a marker
cut :: GammaIndex -> Gamma -> Stack Gamma
cut _ [] = throwError EmptyCut
cut i (x:xs)
  | i == x = return xs
  | otherwise = cut i xs

-- | Look up a type annotated expression
lookupE :: Expr -> Gamma -> Maybe TypeSet
lookupE _ [] = Nothing
lookupE e ((AnnG e' t):gs)
  | e == e' = Just t
  | otherwise = lookupE e gs
lookupE e (_:gs) = lookupE e gs

-- | Look up a solved existential type variable
lookupT :: TVar -> Gamma -> Maybe Type
lookupT _ [] = Nothing
lookupT v ((SolvedG v' t):gs)
  | v == v' = Just t
  | otherwise = lookupT v gs
lookupT v (_:gs) = lookupT v gs

-- | Look up the source of a function
lookupSrc :: (EVar, Language) -> Gamma -> Maybe (EVar, Language, Maybe Path, EVar)
lookupSrc _ [] = Nothing
lookupSrc (e,l) (SrcG x@(e', l', _, _) : rs)
  | e == e' && l == l' = Just x
  | otherwise = lookupSrc (e,l) rs
lookupSrc x (_:rs) = lookupSrc x rs

access1 :: Indexable a => a -> Gamma -> Maybe (Gamma, GammaIndex, Gamma)
access1 gi gs = case DL.elemIndex (index gi) gs of
  (Just 0) -> Just ([], head gs, tail gs)
  (Just i) -> Just (take i gs, gs !! i, drop (i+1) gs)
  _ -> Nothing

access2
  :: (Indexable a)
  => a -> a -> Gamma -> Maybe (Gamma, GammaIndex, Gamma, GammaIndex, Gamma)
access2 lgi rgi gs
  = case access1 lgi gs of
    Just (ls, x, rs) -> case access1 rgi rs of
      Just (ls', y, rs') -> Just (ls, x, ls', y, rs')
      _ -> Nothing
    _ -> Nothing

ann :: Expr -> Type -> Expr
ann (AnnE e _) t = AnnE e t 
ann e@(Declaration _ _) _ = e
ann e@(Signature _ _) _ = e
ann e t = AnnE e t

generalize :: Type -> Type
generalize t = generalize' existentialMap t where 
  generalize' :: [(TVar, TVar)] -> Type -> Type
  generalize' [] t' = t'
  generalize' ((e,r):xs) t' = generalize' xs (generalizeOne e r t')

  existentialMap
    = zip
      (Set.toList (findExistentials t))
      (map (TV . T.pack) variables)

  variables = [1..] >>= flip CM.replicateM ['a'..'z']

  findExistentials :: Type -> Set.Set TVar
  findExistentials UniT = Set.empty
  findExistentials (VarT _) = Set.empty
  findExistentials (ExistT v) = Set.singleton v
  findExistentials (Forall v t') = Set.delete v (findExistentials t')
  findExistentials (FunT t1 t2) = Set.union (findExistentials t1) (findExistentials t2)
  findExistentials (ArrT _ ts) = Set.unions (map findExistentials ts)
  findExistentials (RecT rs) = Set.unions (map (findExistentials . snd) rs)

  generalizeOne :: TVar -> TVar -> Type -> Type
  generalizeOne v0 r t0 = Forall r (f v0 t0) where
    f :: TVar -> Type -> Type
    f v t1@(ExistT v')
      | v == v' = VarT r
      | otherwise = t1
    f v (FunT t1 t2) = FunT (f v t1) (f v t2)
    f v t1@(Forall x t2)
      | v /= x = Forall x (f v t2)
      | otherwise = t1
    f v (ArrT v' xs) = ArrT v' (map (f v) xs)
    f v (RecT xs) = RecT (map (\(v', _) -> (v', f v t)) xs)
    f _ t1 = t1

generalizeE :: Expr -> Expr
generalizeE = mapT generalize

newvar :: Stack Type
newvar = do
  s <- MS.get 
  let v = newvars !! stateVar s
  MS.put $ s {stateVar = stateVar s + 1}
  return (ExistT $ TV v)
  where
    newvars = zipWith (\x y -> T.pack (x ++ show y)) (repeat "t") ([0..] :: [Integer])

newqul :: TVar -> Stack TVar
newqul (TV v) = do
  s <- MS.get
  let v' = TV (v <> "." <> (T.pack . show $ stateQul s)) -- create a new variable such as "a.0"
  MS.put $ s {stateQul = stateQul s + 1}
  return v'


class Indexable a where
  index :: a -> GammaIndex

instance Indexable GammaIndex where
  index = id

instance Indexable Type where
  index (ExistT t) = ExistG t 
  index _ = error "Can only index ExistT"

instance Indexable Expr where
  index (AnnE x t) = AnnG x (fromType t)
  index _ = error "Can only index AnnE"

typeStyle = SetAnsiStyle {
      ansiForeground  = Just (Vivid, Green) -- Set the foreground color, or keep the old one.
    , ansiBackground  = Nothing             -- Set the background color, or keep the old one.
    , ansiBold        = Nothing             -- Switch on boldness, or don’t do anything.
    , ansiItalics     = Nothing             -- Switch on italics, or don’t do anything.
    , ansiUnderlining = Just Underlined     -- Switch on underlining, or don’t do anything.
  } 

instance Pretty MVar where
  pretty (MV t) = pretty t

instance Pretty EVar where
  pretty (EV t) = pretty t

instance Pretty TVar where
  pretty (TV t) = pretty t

prettyMVar :: MVar -> Doc AnsiStyle
prettyMVar (MV x) = pretty x

prettyModule :: Module -> Doc AnsiStyle
prettyModule m
  =  prettyMVar (moduleName m)
  <+> braces (line <> (indent 4 (prettyBlock m)) <> line)

prettyBlock :: Module -> Doc AnsiStyle
prettyBlock m
  =  vsep (map prettyImport (moduleImports m))
  <> vsep ["export" <+> pretty e <> line | (EV e) <- moduleExports m]
  <> vsep (map prettyExpr (moduleBody m))

prettyImport :: Import -> Doc AnsiStyle
prettyImport imp
  = "import" <+> pretty (importModuleName imp)
             <+> maybe "*"
                       (\xs -> encloseSep "(" ")" ", " (map prettyImportOne xs))
                       (importInclude imp)
  where
    prettyImportOne (EV e, EV alias)
      | e /= alias = pretty e
      | otherwise  = pretty e <+> "as" <+> pretty alias

prettyExpr :: Expr -> Doc AnsiStyle
prettyExpr UniE = "()"
prettyExpr (VarE (EV s)) = pretty s
prettyExpr (LamE (EV n) e) = "\\" <> pretty n <+> "->" <+> prettyExpr e
prettyExpr (AnnE e t) = parens (prettyExpr e <+> "::" <+> prettyGreenType t)
prettyExpr (AppE e1@(LamE _ _) e2) = parens (prettyExpr e1) <+> prettyExpr e2
prettyExpr (AppE e1 e2) = prettyExpr e1 <+> prettyExpr e2
prettyExpr (NumE x) = pretty (show x)
prettyExpr (StrE x) = dquotes (pretty x)
prettyExpr (LogE x) = pretty x
prettyExpr (Declaration (EV v) e) = pretty v <+> "=" <+> prettyExpr e
prettyExpr (ListE xs) = list (map prettyExpr xs)
prettyExpr (TupleE xs) = tupled (map prettyExpr xs)
prettyExpr (SrcE lang (Just f) rs)
  = "source" <+> pretty lang <+> "from" <+> pretty f
  <+> tupled (map (\(EV n, EV a) -> pretty n <> if n == a then "" else (" as" <> pretty a)) rs)
prettyExpr (SrcE lang Nothing rs)
  = "source" <+> pretty lang
  <+> tupled (map (\(EV n, EV a) -> pretty n <> if n == a then "" else (" as" <> pretty a)) rs)
prettyExpr (RecE entries) = encloseSep "{" "}" ", " (map (\(EV v,e) -> pretty v <+> "=" <+> prettyExpr e) entries)
prettyExpr (Signature (EV v) e) = pretty v <+> elang' <> "::" <+> eprop' <> etype' <> econs' where 
  elang' :: Doc AnsiStyle
  elang' = maybe "" (\lang -> pretty lang <> " ") (elang e)

  eprop' :: Doc AnsiStyle
  eprop' = case Set.toList (eprop e) of
    [] -> ""
    xs -> tupled (map prettyProperty xs) <+> "=> "

  etype' :: Doc AnsiStyle
  etype' = prettyGreenType (etype e)

  econs' :: Doc AnsiStyle
  econs' = case Set.toList (econs e) of
    [] -> ""
    xs -> " where" <+> tupled (map (\(Con x) -> pretty x) xs)

prettyProperty :: Property -> Doc ann
prettyProperty Pack = "pack"
prettyProperty Unpack = "unpack"
prettyProperty Cast = "cast"
prettyProperty (GeneralProperty ts) = hsep (map pretty ts)

forallVars :: Type -> [Doc a]
forallVars (Forall (TV s) t) = pretty s : forallVars t
forallVars _ = []

forallBlock :: Type -> Doc a
forallBlock (Forall _ t) = forallBlock t
forallBlock t = prettyType t

prettyGreenType :: Type -> Doc AnsiStyle 
prettyGreenType t = annotate typeStyle (prettyType t)

prettyType :: Type -> Doc ann
prettyType UniT = "1"
prettyType (VarT (TV s)) = pretty s
prettyType (FunT t1@(FunT _ _) t2) = parens (prettyType t1) <+> "->" <+> prettyType t2
prettyType (FunT t1 t2) = prettyType t1 <+> "->" <+> prettyType t2
prettyType t@(Forall _ _) = "forall" <+> hsep (forallVars t) <+> "." <+> forallBlock t
prettyType (ExistT (TV e)) = "<" <> pretty e <> ">"
prettyType (ArrT (TV v) ts) = pretty v <+> hsep (map prettyType ts)
prettyType (RecT entries) = encloseSep "{" "}" ", " (map (\(TV v,e) -> pretty v <+> "=" <+> prettyType e) entries)

class Describable a where
  desc :: a -> String

instance Describable Expr where
  desc (UniE) = "UniE"
  desc (VarE (EV v)) = "VarE:" ++ T.unpack v
  desc (ListE _) = "ListE"
  desc (TupleE _) = "Tuple"
  desc (SrcE _ _ _) = "SrcE:"
  desc (LamE (EV v) _) = "LamE:" ++ T.unpack v
  desc (AppE e1 e2) = "AppE (" ++ desc e1 ++ ") (" ++ desc e2 ++ ")"
  desc (AnnE e _) = "AnnE (" ++ desc e ++ ")"
  desc (NumE x) = "NumE:" ++ show x
  desc (LogE x) = "LogE:" ++ show x
  desc (StrE x) = "StrE:" ++ show x
  desc (RecE _) = "RecE:"
  desc (Declaration (EV e) _) = "Declaration:" ++ T.unpack e
  desc (Signature (EV e) _) = "Signature:" ++ T.unpack e

instance Describable Type where
  desc (UniT) = "UniT"
  desc (VarT (TV v)) = "VarT:" ++ T.unpack v
  desc (ExistT (TV v)) = "ExistT:" ++ T.unpack v
  desc (Forall (TV v) _) = "Forall:" ++ T.unpack v
  desc (FunT t1 t2) = "FunT (" ++ desc t1 ++ ") (" ++ desc t2 ++ ")"
  desc (ArrT (TV v) xs) = "ArrT:" ++ T.unpack v ++ " " ++ (concat . map desc) xs
  desc (RecT _)  = "RecT:"
