{-# LANGUAGE OverloadedStrings #-}

module Morloc.Frontend.CST
  ( Span(..)
  , Loc(..)
  , HasPos(..)
  , CstExpr(..)
  , CstExport(..)
  , CstSigType(..)
  , CstTypeDef(..)
  , CstClassHead(..)
  , CstSigItem(..)
  , CstDoStmt(..)
  , CstAccessorBody(..)
  , CstAccessorTail(..)
  , at
  , (<->)
  , valOf
  ) where

import Data.Text (Text)
import Data.Scientific (Scientific)
import Morloc.Frontend.Token (Located(..), Pos(..))
import Morloc.Namespace.Prim
import Morloc.Namespace.Type (TypeU(..), NamType(..), Constraint(..))
import Morloc.Namespace.Expr (Import(..), Associativity(..))

-- | Source span: start position to end position
data Span = Span !Pos !Pos
  deriving (Show, Eq)

-- | Span-annotated wrapper
data Loc a = Loc !Span a
  deriving (Show, Eq)

valOf :: Loc a -> a
valOf (Loc _ x) = x

-- | Wrap a value at a single token's position
at :: Located -> a -> Loc a
at tok x = Loc (Span p p) x where p = locPos tok

-- | Build a span between any two things that have positions.
-- Works with Located tokens and Loc-wrapped values.
class HasPos a where
  startPos :: a -> Pos
  endPos :: a -> Pos

instance HasPos Located where
  startPos = locPos
  endPos = locPos

instance HasPos (Loc a) where
  startPos (Loc (Span s _) _) = s
  endPos (Loc (Span _ e) _) = e

instance HasPos Span where
  startPos (Span s _) = s
  endPos (Span _ e) = e

-- | Build a span from a start-positioned thing to an end-positioned thing
(<->) :: (HasPos a, HasPos b) => a -> b -> Span
a <-> b = Span (startPos a) (endPos b)

infixl 5 <->

--------------------------------------------------------------------
-- CST node types
--------------------------------------------------------------------

data CstExpr
  -- Top-level declarations
  = CModE Text CstExport [Loc CstExpr]
  | CImpE Import
  | CSigE EVar [Text] CstSigType
  | CAssE EVar [Text] (Loc CstExpr) [Loc CstExpr]
  | CTypE CstTypeDef
  | CClsE CstClassHead [CstSigItem]
  | CIstE ClassName [TypeU] [Loc CstExpr]
  | CFixE Associativity Int [EVar]
  | CSrcOldE Located (Maybe Text) [(Text, Maybe Text)]
  | CSrcNewE Located (Maybe Text) [Located]
  -- Expressions
  | CAppE (Loc CstExpr) [Loc CstExpr]
  | CLamE [EVar] (Loc CstExpr)
  | CLetE [(EVar, Loc CstExpr)] (Loc CstExpr)
  | CBopE (Loc CstExpr) Located (Loc CstExpr)
  | CVarE EVar
  | CIntE Integer
  | CRealE Scientific
  | CStrE Text
  | CLogE Bool
  | CUniE
  | CHolE
  | CLstE [Loc CstExpr]
  | CTupE [Loc CstExpr]
  | CNamE [(Key, Loc CstExpr)]
  | CSuspendE (Loc CstExpr)
  | CForceE (Loc CstExpr)
  | CAnnE (Loc CstExpr) TypeU
  | CDoE [CstDoStmt]
  | CAccessorE CstAccessorBody
  | CInterpE Text [Loc CstExpr] [Text] Text
  deriving (Show, Eq)

data CstExport
  = CstExportAll
  | CstExportMany [Located]
  deriving (Show, Eq)

data CstSigType = CstSigType
  { cstSigConstraintArgs :: Maybe [(Pos, TypeU)]
  , cstSigArgs :: [(Pos, TypeU)]
  }
  deriving (Show, Eq)

data CstTypeDef
  = CstTypeAlias (Maybe Located) (TVar, [Either TVar TypeU]) (TypeU, Bool)
  | CstTypeAliasForward (TVar, [Either TVar TypeU])
  | CstNamTypeWhere NamType (TVar, [Either TVar TypeU]) [(Located, Key, TypeU)]
  | CstNamTypeLegacy (Maybe Located) NamType (TVar, [Either TVar TypeU]) (Text, Bool) [(Key, TypeU)]
  deriving (Show, Eq)

data CstClassHead
  = CCHSimple TypeU
  | CCHConstrained TypeU TypeU
  | CCHMultiConstrained [Constraint] TypeU
  deriving (Show, Eq)

data CstSigItem = CstSigItem EVar [Text] CstSigType
  deriving (Show, Eq)

data CstDoStmt
  = CstDoBind EVar (Loc CstExpr)
  | CstDoBare (Loc CstExpr)
  deriving (Show, Eq)

data CstAccessorBody
  = CABKey Text CstAccessorTail
  | CABIdx Int CstAccessorTail
  | CABGroup [CstAccessorBody]
  deriving (Show, Eq)

data CstAccessorTail
  = CATEnd
  | CATSet (Loc CstExpr)
  | CATChain CstAccessorBody
  deriving (Show, Eq)
