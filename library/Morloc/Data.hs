module Morloc.Data (
    MData(..)
  , MType(..)
) where

data MData
  = MInt       Integer
  | MNum       Double
  | MString    String
  | MBool      Bool
  -- TODO: add nested arrays 
  | MInts    [ Integer ]
  | MNums    [ Double  ]
  | MStrings [ String  ]
  | MBools   [ Bool    ]
  | MFunc String -- for now, MFunc is just a function name 
  deriving (Eq, Ord, Show)

-- store a Morloc type
data MType = MType
  String  -- type identifier, e.g. Matrix
  [MType] -- type parameters, e.g. Number
  deriving (Eq, Ord, Show)
