-- I want to translate the entire Morloc program into an RDF triplet store.

module Morloc.Triple (
    Triple(..)
  , Subject
  , RelObj(..)
  , Thing(..)
  , Primitive(..)
) where

import Morloc.Syntax
import Morloc.Data

type Triple = (Subject, RelObj)

type Subject = Int

data RelObj
  = IsA'     Thing
  | Name'    String
  | Parent'  Subject
  | Lang'    String
  | File'    String
  | Alias'   String
  | Role'    String
  | Cons'    Subject
  | Binop'   Subject
  | LHS'     Subject
  | RHS'     Subject
  | Appl'    [Subject]
  | Args'    [Subject]
  | Output'  Subject
  | Params'  [Subject]
  | Value'   Primitive 
  deriving (Ord, Eq)

showArray :: Show a => [a] -> String
showArray xs = "(" ++ unwords (map show xs) ++ ")"

instance Show RelObj where
  show (IsA'    x)  = unwords [ ":isa"    , show x       ]
  show (Name'   x)  = unwords [ ":name"   , x            ]
  show (Parent' x)  = unwords [ ":parent" , show x       ]
  show (Lang'   x)  = unwords [ ":lang"   , x            ]
  show (File'   x)  = unwords [ ":file"   , x            ]
  show (Alias'  x)  = unwords [ ":alias"  , x            ]
  show (Role'   x)  = unwords [ ":role"   , x            ]
  show (Cons'   x)  = unwords [ ":cons"   , show x       ]
  show (Binop'  x)  = unwords [ ":binop"  , show x       ]
  show (LHS'    x)  = unwords [ ":lhs"    , show x       ]
  show (RHS'    x)  = unwords [ ":rhs"    , show x       ]
  show (Appl'   xs) = unwords [ ":appl"   , showArray xs ]
  show (Args'   xs) = unwords [ ":args"   , showArray xs ]
  show (Params' xs) = unwords [ ":param"  , showArray xs ]
  show (Value'  x)  = unwords [ ":value"  , show x       ]

data Thing
  = Signature'
  | Declaration'
  | Import'
  | Module'
  | List'
  | Tuple'
  | Record'
  deriving (Ord, Eq)

instance Show Thing where
  show Signature'   = ":Signature"
  show Declaration' = ":declaration"
  show Import'      = ":import"
  show Module'      = ":module"
  show List'        = ":list"
  show Tuple'       = ":tuple"
  show Record'      = ":record"

data Primitive 
  = Int' Integer
  | Num' Double
  | Log' Bool
  | Str' String
  | Arr' [Subject]
  deriving (Ord, Eq)

instance Show Primitive where
  show (Int' x ) = show x
  show (Num' x ) = show x
  show (Log' x ) = if x then "true" else "false"
  show (Str' x ) = x
  show (Arr' xs) = showArray xs
