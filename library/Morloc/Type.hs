module Morloc.Type
(
    Type(..)
  , Common(..)
  , Lang(..)
  , validateE
  , generateO
  , generateI
  , convertE
) where



import Text.ParserCombinators.ReadP (pfail)
import qualified Text.Read.Lex as L
import qualified GHC.Read as R


{- data ColumnSpec = ColumnSpec {name    :: String       , kind     :: Type} deriving (Show,Read) -}
{- data TableSpec  = TableSpec  {columns :: [ColumnSpec] , rownames :: Bool} deriving (Show,Read) -}
{- data HashSpec   = HashSpec   {key     :: Type         , value    :: Type} deriving (Show,Read) -}
{- data TreeSpec   = TreeSpec   {leaf    :: Type         , node     :: Type} deriving (Show,Read) -}

data Type 
  = MVoid
  | MInt
  | MNum
  | MChar
  | MString
  | MBool
  | MText
  | MFile
  | MBinary
  | MTable  --TableSpec
  | MMatrix --Type
  | MVector --Type
  | MTuple  --[Type]
  | MSet    --Type
  | MHash   --HashSpec
  | MTree   --TreeSpec
  deriving (Show, Read)

data Lang
  = R
  | Bash
  | Haskell
  deriving(Read)

instance Show Lang where
  show R       = "R"
  show Bash    = "sh"
  show Haskell = "hs"

{- instance Read Lang where               -}
{-   readPrec =                           -}
{-     parens                             -}
{-     ( do L.Ident s <- R.lexP           -}
{-          case s of                     -}
{-            "R"  -> return R            -}
{-            "sh" -> return Bash         -}
{-            "hs" -> return Haskell      -}
{-            _    -> pfail               -}
{-     )                                  -}
{-   readListPrec = R.readListPrecDefault -}
{-   readList     = R.readListDefault     -}

{- instance Read Type where               -}
{-   readPrec =                           -}
{-     parens                             -}
{-     ( do L.Ident s <- R.lexP           -}
{-          case s of                     -}
{-            "Int"    -> return MInt     -}
{-            "String" -> return MString  -}
{-            _        -> pfail           -}
{-     )                                  -}
{-   readListPrec = R.readListPrecDefault -}
{-   readList     = R.readListDefault     -}

type Key   = String
type Value = String
type Code  = String
type TypeSpec = [(Lang, Key, Value)]
type EdgeSpec = [(Key, Value)]

data Common = Common { ctype :: String , cvalue :: String } | Raw String deriving(Show)

{- instance FromJSON Common where                                             -}
{-   parseJSON (Object v) = Common                                            -}
{-     <$> v .: "type"                                                        -}
{-     <*> v .: "value"                                                       -}


-- ========================================================================= --
--------------------------- C O N V E R T E R S -------------------------------
-- ========================================================================= --
convertE :: Type -> Type -> EdgeSpec -> Common -> Maybe Common
-------------------------------------------------------------------------------

convertE _ _ _ _ = Nothing


-- ========================================================================= --
-------------------- O U T P U T   G E N E R A T O R S ------------------------
-- ========================================================================= --
generateI :: Lang -> Type -> TypeSpec -> Maybe Code
-------------------------------------------------------------------------------

generateI Bash MNum    _ = Nothing
generateI Bash MChar   _ = Nothing
generateI Bash MString _ = Nothing
generateI Bash MBool   _ = Nothing
generateI Bash MText   _ = Nothing
generateI Bash MFile   _ = Nothing
generateI Bash _       _ = Nothing


generateI Haskell _ _ = Nothing

generateI R       _ _ = Nothing 



-- ========================================================================= --
--------------------- I N P U T   G E N E R A T O R S -------------------------
-- ========================================================================= --
generateO :: Lang -> Type -> TypeSpec -> Maybe Code
-------------------------------------------------------------------------------

generateO Bash    _ _ = Nothing
generateO Haskell _ _ = Nothing
generateO R       _ _ = Nothing



-- ========================================================================= --
---------------------- E D G E   V A L I D A T O R S --------------------------
-- ========================================================================= --
validateE :: Type -> EdgeSpec -> Common -> Bool 
-------------------------------------------------------------------------------

{- validateE MInt    _ (Raw x) =                -}
{-     case (reads x :: [(Int, String)]) of     -}
{-         [(a, "")] -> True                    -}
{-         _         -> False                   -}
{-                                              -}
{- validateE MNum    _ (Raw x) =                -}
{-     case (reads x :: [(Float, String)]) of   -}
{-         [(a, "")] -> True                    -}
{-         _         -> False                   -}
{-                                              -}
{- validateE MChar    _ (Raw x) = length x == 1 -}
{- validateE MBool    _ (Raw x)                 -}
{-     | x == "true"  = True                    -}
{-     | x == "false" = True                    -}
{-     | otherwise    = False                   -}

validateE MInt    _ _ = True
validateE MNum    _ _ = True
validateE MChar   _ _ = True
validateE MBool   _ _ = True
validateE MVoid   _ _ = True
validateE MString _ _ = True
validateE MText   _ _ = True
validateE MFile   _ _ = True
validateE _       _ _ = False
