{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.Namespace.Expr
Description : Frontend AST, source/config types, post-typecheck tree
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}

module Morloc.Namespace.Expr
  ( -- * Source and config types
    Source (..)
  , RemoteResources (..)
  , ManifoldConfig (..)
  , ModuleConfig (..)
  , BuildConfig (..)

    -- * Mostly frontend expressions
  , Symbol (..)
  , AliasedSymbol (..)
  , Signature (..)
  , Typeclass (..)
  , Selector (..)
  , ungroup
  , Pattern (..)
  , Expr (..)
  , ExprI (..)
  , E (..)
  , Lit (..)
  , Import (..)
  , Export (..)
  , Fixity (..)
  , Associativity (..)

    -- * Post-typechecking tree
  , ExecutableExpr (..)
  , AnnoS (..)
  , ExprS (..)
  , ManyPoly (..)
  , mapAnnoSM
  , mapExprSM
  , mapAnnoS
  , mapExprS
  , mapAnnoSC
  , mapAnnoSCM
  , mapAnnoSG
  , mapAnnoSGM
  , mapExprSC
  , mapExprSCM
  , mapExprSG
  , mapExprSGM

    -- * JSON helpers
  , stripPrefixAndKebabCase
  , convertToKebabCase
  ) where

import Control.Monad.Identity (runIdentity)
import Data.Aeson (FromJSON (..))
import Data.Aeson.Types (Options (..), defaultOptions)
import qualified Data.Aeson as Aeson
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Scientific (Scientific)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Morloc.Data.Doc
import Morloc.Namespace.Prim
import Morloc.Namespace.Type

---- Source and config types

data Source
  = Source
  { srcName :: SrcName
  , srcLang :: Lang
  , srcPath :: Maybe Path
  , srcAlias :: EVar
  , srcLabel :: Maybe Label
  , srcRsize :: [Int]
  , srcNote :: [Text]
  }
  deriving (Ord, Eq, Show)

data RemoteResources = RemoteResources
  { remoteResourcesThreads :: Maybe Int
  , remoteResourcesMemory :: Maybe Int
  , remoteResourcesTime :: Maybe TimeInSeconds
  , remoteResourcesGpus :: Maybe Int
  }
  deriving (Show, Ord, Eq, Generic)

data ManifoldConfig = ManifoldConfig
  { manifoldConfigCache :: Maybe Bool
  , manifoldConfigBenchmark :: Maybe Bool
  , manifoldConfigRemote :: Maybe RemoteResources
  }
  deriving (Show, Ord, Eq, Generic)

data ModuleConfig = ModuleConfig
  { moduleConfigDefaultGroup :: Maybe ManifoldConfig
  , moduleConfigLabeledGroups :: Map.Map Text ManifoldConfig
  }
  deriving (Show, Generic)

data BuildConfig = BuildConfig
  { buildConfigSlurmSupport :: Maybe Bool
  }
  deriving (Show, Generic)

---- Expressions

data Symbol
  = TypeSymbol TVar
  | TermSymbol EVar
  | ClassSymbol ClassName
  deriving (Show, Ord, Eq)

data Export = ExportMany (Set.Set (Int, Symbol)) | ExportAll
  deriving (Show, Ord, Eq)

data AliasedSymbol
  = AliasedType TVar TVar
  | AliasedTerm EVar EVar
  | AliasedClass ClassName
  deriving (Show, Ord, Eq)

data Signature = Signature EVar (Maybe Label) EType
  deriving (Show, Ord, Eq)

data Typeclass a = Typeclass ClassName [TVar] [a]
  deriving (Show, Ord, Eq)

data Selector
  = SelectorKey (Text, Selector) [(Text, Selector)]
  | SelectorIdx (Int, Selector) [(Int, Selector)]
  | SelectorEnd
  deriving (Show, Ord, Eq)

ungroup :: Selector -> [[Either Int Text]]
ungroup SelectorEnd = [[]]
ungroup (SelectorKey (k, SelectorEnd) []) = [[Right k]]
ungroup (SelectorIdx (i, SelectorEnd) []) = [[Left i]]
ungroup (SelectorKey x xs) = concat [map ((:) (Right k)) (ungroup s) | (k, s) <- (x : xs)]
ungroup (SelectorIdx x xs) = concat [map ((:) (Left i)) (ungroup s) | (i, s) <- (x : xs)]

data Pattern
  = PatternText Text [Text]
  | PatternStruct Selector
  deriving (Show, Ord, Eq)

data ExprI = ExprI Int Expr
  deriving (Show, Ord, Eq)

data Expr
  = ModE MVar [ExprI]
  | ClsE (Typeclass Signature)
  | IstE ClassName [TypeU] [ExprI]
  | TypE ExprTypeE
  | ImpE Import
  | ExpE Export
  | SrcE Source
  | SigE Signature
  | AssE EVar ExprI [ExprI]
  | FixE Fixity
  | BopE ExprI Int EVar ExprI
  | UniE
  | VarE ManifoldConfig EVar
  | HolE
  | LstE [ExprI]
  | TupE [ExprI]
  | NamE [(Key, ExprI)]
  | AppE ExprI [ExprI]
  | LamE [EVar] ExprI
  | AnnE ExprI TypeU
  | RealE Scientific
  | IntE Integer
  | LogE Bool
  | StrE Text
  | PatE Pattern
  deriving (Show, Ord, Eq)

data Import
  = Import
  { importModuleName :: MVar
  , importInclude :: Maybe [AliasedSymbol]
  , importExclude :: [Symbol]
  , importNamespace :: Maybe EVar
  }
  deriving (Ord, Eq, Show)

data Associativity
  = InfixL
  | InfixR
  | InfixN
  deriving (Show, Ord, Eq, Enum)

data Fixity = Fixity
  { fixityAssoc :: Associativity
  , fixityPrecedence :: Int
  , fixityOperators :: [EVar]
  }
  deriving (Show, Ord, Eq)

data Lit
  = MNum Scientific
  | MInt Integer
  | MLog Bool
  | MStr Text
  | MUni
  deriving (Ord, Eq, Show)

data E
  = BndP (Indexed Type) EVar
  | VarP (Indexed Type) EVar [E]
  | AppP (Indexed Type) E [E]
  | LamP (Indexed Type) [EVar] E
  | LstP (Indexed Type) [E]
  | TupP (Indexed Type) [E]
  | NamP (Indexed Type) [(Key, E)]
  | LitP (Indexed Type) Lit
  | SrcP (Indexed Type) Source
  | PatP (Indexed Type) Selector
  deriving (Ord, Eq, Show)

data ExecutableExpr = SrcCall Source | PatCall Pattern
  deriving (Ord, Eq, Show)

data AnnoS g f c = AnnoS g c (ExprS g f c)

data ExprS g f c
  = UniS
  | BndS EVar
  | VarS EVar (f (AnnoS g f c))
  | AppS (AnnoS g f c) [AnnoS g f c]
  | LamS [EVar] (AnnoS g f c)
  | LstS [AnnoS g f c]
  | TupS [AnnoS g f c]
  | NamS [(Key, AnnoS g f c)]
  | RealS Scientific
  | IntS Integer
  | LogS Bool
  | StrS Text
  | ExeS ExecutableExpr

data ManyPoly a = MonomorphicExpr (Maybe EType) [a] | PolymorphicExpr ClassName EVar EType [(EType, [a])]
  deriving (Show, Eq, Ord)

---- Class instances

instance HasOneLanguage Source where
  langOf s = Just (srcLang s)
  langOf' s = srcLang s

instance Functor ManyPoly where
  fmap f (MonomorphicExpr t xs) = MonomorphicExpr t (map f xs)
  fmap f (PolymorphicExpr cls v t xs) = PolymorphicExpr cls v t (map (second (map f)) xs)

instance Traversable ManyPoly where
  traverse f (MonomorphicExpr t xs) = MonomorphicExpr t <$> traverse f xs
  traverse f (PolymorphicExpr cls v t xs) = PolymorphicExpr cls v t <$> traverse f2 xs
    where
      f2 (t', x) = (,) t' <$> traverse f x

instance Foldable ManyPoly where
  foldr f b (MonomorphicExpr _ xs) = foldr f b xs
  foldr f b (PolymorphicExpr _ _ _ (concatMap snd -> xs)) = foldr f b xs

instance Defaultable ModuleConfig where
  defaultValue =
    ModuleConfig
      { moduleConfigDefaultGroup = Nothing
      , moduleConfigLabeledGroups = Map.empty
      }

instance Defaultable BuildConfig where
  defaultValue =
    BuildConfig
      { buildConfigSlurmSupport = Nothing
      }

instance Defaultable RemoteResources where
  defaultValue =
    RemoteResources
      { remoteResourcesThreads = Nothing
      , remoteResourcesMemory = Nothing
      , remoteResourcesTime = Nothing
      , remoteResourcesGpus = Nothing
      }

instance Defaultable ManifoldConfig where
  defaultValue =
    ManifoldConfig
      { manifoldConfigCache = Just False
      , manifoldConfigBenchmark = Just False
      , manifoldConfigRemote = Nothing
      }

instance FromJSON ModuleConfig where
  parseJSON =
    Aeson.genericParseJSON $
      defaultOptions {fieldLabelModifier = stripPrefixAndKebabCase "moduleConfig"}

instance FromJSON ManifoldConfig where
  parseJSON =
    Aeson.genericParseJSON $
      defaultOptions {fieldLabelModifier = stripPrefixAndKebabCase "manifoldConfig"}

instance FromJSON RemoteResources where
  parseJSON =
    Aeson.genericParseJSON $
      defaultOptions {fieldLabelModifier = stripPrefixAndKebabCase "remoteResources"}

instance FromJSON BuildConfig where
  parseJSON =
    Aeson.genericParseJSON $
      defaultOptions {fieldLabelModifier = stripPrefixAndKebabCase "buildConfig"}

---- JSON helpers

-- Helper function to strip prefixes and convert to kebab-case
stripPrefixAndKebabCase :: String -> String -> String
stripPrefixAndKebabCase prefix str =
  let stripped = drop (length prefix) str
   in case stripped of
        [] -> []
        (x : xs) -> toLower x : convertToKebabCase xs

-- Convert remaining characters to kebab-case
convertToKebabCase :: String -> String
convertToKebabCase [] = []
convertToKebabCase (x : xs)
  | isUpper x = '-' : toLower x : convertToKebabCase xs
  | otherwise = x : convertToKebabCase xs

---- Helper functions for AnnoS/ExprS

mapExprSM ::
  (Traversable f, Monad m) => (AnnoS g f c -> m (AnnoS g' f c')) -> ExprS g f c -> m (ExprS g' f c')
mapExprSM f (VarS v xs) = VarS v <$> traverse f xs
mapExprSM f (AppS x xs) = AppS <$> f x <*> mapM f xs
mapExprSM f (LamS vs x) = LamS vs <$> f x
mapExprSM f (LstS xs) = LstS <$> mapM f xs
mapExprSM f (TupS xs) = TupS <$> mapM f xs
mapExprSM f (NamS rs) = NamS <$> mapM (secondM f) rs
mapExprSM _ UniS = return UniS
mapExprSM _ (BndS v) = return $ BndS v
mapExprSM _ (RealS x) = return $ RealS x
mapExprSM _ (IntS x) = return $ IntS x
mapExprSM _ (LogS x) = return $ LogS x
mapExprSM _ (StrS x) = return $ StrS x
mapExprSM _ (ExeS x) = return $ ExeS x

mapAnnoSM ::
  (Traversable f, Monad m) =>
  (ExprS g f c -> g -> c -> m (g', c')) ->
  AnnoS g f c ->
  m (AnnoS g' f c')
mapAnnoSM fun (AnnoS g c e) = do
  e' <- mapExprSM (mapAnnoSM fun) e
  (g', c') <- fun e g c
  return (AnnoS g' c' e')

mapAnnoS :: (Traversable f) => (ExprS g f c -> g -> c -> (g', c')) -> AnnoS g f c -> AnnoS g' f c'
mapAnnoS fun = runIdentity . mapAnnoSM (\x g c -> return (fun x g c))

mapExprS :: (Traversable f) => (AnnoS g f c -> AnnoS g' f c') -> ExprS g f c -> ExprS g' f c'
mapExprS fun = runIdentity . mapExprSM (return . fun)

mapAnnoSGM :: (Traversable f, Monad m) => (g -> m g') -> AnnoS g f c -> m (AnnoS g' f c)
mapAnnoSGM f = mapAnnoSM (\_ gi ci -> (,) <$> f gi <*> pure ci)

mapAnnoSCM :: (Traversable f, Monad m) => (c -> m c') -> AnnoS g f c -> m (AnnoS g f c')
mapAnnoSCM f = mapAnnoSM (\_ gi ci -> (,) gi <$> f ci)

mapAnnoSG :: (Traversable f) => (g -> g') -> AnnoS g f c -> AnnoS g' f c
mapAnnoSG f = mapAnnoS (\_ gi ci -> (f gi, ci))

mapAnnoSC :: (Traversable f) => (c -> c') -> AnnoS g f c -> AnnoS g f c'
mapAnnoSC f = mapAnnoS (\_ gi ci -> (gi, f ci))

mapExprSGM :: (Traversable f, Monad m) => (g -> m g') -> ExprS g f c -> m (ExprS g' f c)
mapExprSGM f = mapExprSM (\(AnnoS gi ci e) -> AnnoS <$> f gi <*> pure ci <*> mapExprSGM f e)

mapExprSCM :: (Traversable f, Monad m) => (c -> m c') -> ExprS g f c -> m (ExprS g f c')
mapExprSCM f = mapExprSM (\(AnnoS gi ci e) -> AnnoS gi <$> f ci <*> mapExprSCM f e)

mapExprSG :: (Traversable f) => (g -> g') -> ExprS g f c -> ExprS g' f c
mapExprSG f = mapExprS (\(AnnoS gi ci e) -> AnnoS (f gi) ci (mapExprSG f e))

mapExprSC :: (Traversable f) => (c -> c') -> ExprS g f c -> ExprS g f c'
mapExprSC f = mapExprS (\(AnnoS gi ci e) -> AnnoS gi (f ci) (mapExprSC f e))

----- Pretty instances -------------------------------------------------------

instance Pretty Lit where
  pretty (MNum x) = viaShow x
  pretty (MInt x) = pretty x
  pretty (MLog x) = pretty x
  pretty (MStr x) = pretty x
  pretty MUni = "Unit"

instance Pretty E where
  pretty (BndP _ v) = pretty v
  pretty (VarP _ v _) = pretty v
  pretty (AppP _ e es) = pretty e <+> hsep (map f es)
    where
      f x@AppP {} = parens (pretty x)
      f x@LamP {} = parens (pretty x)
      f x@SrcP {} = parens (pretty x)
      f x = pretty x
  pretty (LamP _ vs e) = "\\" <+> hsep (map pretty vs) <+> "->" <+> pretty e
  pretty (LstP _ es) = list (map pretty es)
  pretty (TupP _ es) = tupled (map pretty es)
  pretty (NamP _ rs) = encloseSep "{" "}" "," [pretty k <+> "=" <+> pretty e | (k, e) <- rs]
  pretty (LitP _ l) = pretty l
  pretty (SrcP _ src) = pretty src
  pretty (PatP _ s) = pretty (PatternStruct s)

instance Pretty Source where
  pretty s =
    "source" <+> pretty (srcLang s)
      <> maybe "" (\path -> " from" <+> dquotes (pretty path)) (srcPath s)
        <+> dquotes (pretty (srcName s))
        <+> "as"
        <+> pretty (srcAlias s)
      <> maybe "" (\t -> ":" <> pretty t) (srcLabel s)

instance Pretty Symbol where
  pretty (TypeSymbol x) = pretty x
  pretty (TermSymbol x) = pretty x
  pretty (ClassSymbol x) = pretty x

instance Pretty AliasedSymbol where
  pretty (AliasedType x alias)
    | x == alias = pretty x
    | otherwise = pretty x <+> "as" <+> pretty alias
  pretty (AliasedTerm x alias)
    | x == alias = pretty x
    | otherwise = pretty x <+> "as" <+> pretty alias
  pretty (AliasedClass x) = pretty x

instance Pretty ExprI where
  pretty (ExprI i e) = parens (pretty e) <> ":" <> pretty i

instance Pretty Pattern where
  pretty (PatternText s ss) = dquotes $ hcat (pretty s : ["#{}" <> pretty s' | s' <- ss])
  pretty (PatternStruct s) = pretty s

instance Pretty Selector where
  pretty SelectorEnd = ""
  pretty (SelectorKey (k, s) []) = "." <> pretty k <> pretty s
  pretty (SelectorIdx (i, s) []) = "." <> pretty i <> pretty s
  pretty (SelectorKey r rs) = "." <> tupled ["." <> pretty k <> pretty s | (k, s) <- (r : rs)]
  pretty (SelectorIdx r rs) = "." <> tupled ["." <> pretty i <> pretty s | (i, s) <- (r : rs)]

instance Pretty Expr where
  pretty HolE = "_"
  pretty (PatE pat) = "pattern:" <+> pretty pat
  pretty UniE = "()"
  pretty (ModE v es) = align . vsep $ ("module" <+> pretty v) : map pretty es
  pretty (ClsE (Typeclass cls vs sigs)) = "class" <+> pretty cls <+> hsep (map pretty vs) <> (align . vsep . map pretty) sigs
  pretty (IstE cls ts es) = "instance" <+> pretty cls <+> hsep (map (parens . pretty) ts) <> (align . vsep . map pretty) es
  pretty (TypE (ExprTypeE lang v vs t _)) =
    "type" <+> pretty lang
      <> "@"
      <> pretty v
        <+> sep (map (either pretty (parens . pretty)) vs)
        <+> "="
        <+> pretty t
  pretty (ImpE (Import m Nothing _ _)) = "import" <+> pretty m
  pretty (ImpE (Import m (Just xs) _ _)) = "import" <+> pretty m <+> tupled (map pretty xs)
  pretty (ExpE ExportAll) = "export *"
  pretty (ExpE (ExportMany symbols)) = "export" <+> tupled (map pretty (Set.toList symbols))
  pretty (VarE _ s) = pretty s
  pretty (LamE v e) = "\\" <+> pretty v <+> "->" <+> pretty e
  pretty (AnnE e t) = parens (pretty e <+> "::" <+> pretty t)
  pretty (LstE es) = encloseSep "[" "]" "," (map pretty es)
  pretty (TupE es) = encloseSep "[" "]" "," (map pretty es)
  pretty (AppE f es) = vsep (map pretty (f : es))
  pretty (NamE rs) = block 4 "<RECORD>" (vsep [pretty k <+> "::" <+> pretty x | (k, x) <- rs])
  pretty (RealE x) = pretty (show x)
  pretty (IntE x) = pretty (show x)
  pretty (StrE x) = dquotes (pretty x)
  pretty (LogE x) = pretty x
  pretty (AssE v e es) = pretty v <+> "=" <+> pretty e <+> "where" <+> (align . vsep . map pretty) es
  pretty (SrcE (Source srcname lang file' alias _ rsizes _)) =
    "source"
      <+> viaShow lang
      <> maybe "" (\f -> "from" <+> pretty f) file'
        <+> "where\n"
      <> indent
           2
           ( vsep
               [ "--' srcname: " <> pretty srcname
               , "--' rsize: " <> encloseSep "" "" " " (map pretty rsizes)
               , pretty alias
               ]
           )
  pretty (SigE (Signature v _ e)) =
    pretty v <+> "::" <+> eprop' <> etype' <> econs'
    where
      eprop' :: Doc ann
      eprop' =
        case Set.toList (eprop e) of
          [] -> ""
          xs -> tupled (map pretty xs) <+> "=> "
      etype' :: Doc ann
      etype' = pretty (etype e)
      econs' :: Doc ann
      econs' =
        case Set.toList (econs e) of
          [] -> ""
          xs -> " where" <+> tupled (map (\(Con x) -> pretty x) xs)
  pretty (FixE (Fixity assoc prec ops)) =
    assocStr <+> pretty prec <+> hsep (map pretty ops)
    where
      assocStr :: Doc ann
      assocStr = case assoc of
        InfixL -> "infixl"
        InfixR -> "infixr"
        InfixN -> "infix"
  pretty (BopE e1 _ v e2) = pretty e1 <+> pretty v <+> pretty e2

instance (Foldable f) => Pretty (AnnoS a f b) where
  pretty (AnnoS _ _ e) = pretty e

instance (Foldable f) => Pretty (ExprS a f b) where
  pretty (AppS e es) = "(AppS" <+> list (map pretty (e : es)) <> ")"
  pretty (VarS v res) = "(VarS" <+> pretty v <+> "=" <+> list (map pretty (toList res)) <> ")"
  pretty (LamS vs e) = "(LamS" <+> list (map pretty vs) <+> "->" <+> pretty e <> ")"
  pretty (LstS es) = "(LstS" <+> list (map pretty es) <> ")"
  pretty (TupS es) = "(TupS" <+> list (map pretty es) <> ")"
  pretty (NamS rs) = "(NamS" <+> list [pretty k <> "=" <> pretty v | (k, v) <- rs] <> ")"
  pretty UniS = "UniS"
  pretty (BndS x) = "(BndS" <+> pretty x <> ")"
  pretty (RealS x) = viaShow x
  pretty (IntS x) = viaShow x
  pretty (LogS x) = viaShow x
  pretty (StrS x) = viaShow x
  pretty (ExeS x) = pretty x

instance Pretty ExecutableExpr where
  pretty (SrcCall src) = pretty src
  pretty (PatCall pat) = pretty pat

instance Pretty Signature where
  pretty (Signature v _ e) = pretty v <+> "::" <+> pretty (etype e)
