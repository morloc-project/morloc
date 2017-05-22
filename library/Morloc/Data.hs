module Morloc.Data (MData(..)) where

data MData
  = MInt       Integer
  | MNum       Double
  | MString    String
  | MBool      Bool
  {- | MInts    [ Integer ] -}
  {- | MNums    [ Double  ] -}
  {- | MStrings [ String  ] -}
  {- | MBools   [ Bool    ] -}
  | MFunc String -- for now, MFunc is just a function name 
  deriving (Eq, Ord, Show)
