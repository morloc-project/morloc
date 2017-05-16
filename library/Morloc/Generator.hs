module Morloc.Generator
(
    Type(..)
  , Common(..)
  , validateE
  , generateO
  , generateI
  , convertE
) where



import Text.ParserCombinators.ReadP (pfail)
import qualified Text.Read.Lex as L
import qualified GHC.Read as R

-- Eventually I will need to use JSON, for that I can use the Aeson library:
-- http://hackage.haskell.org/package/aeson-1.2.0.0/docs/Data-Aeson.html
-- But for now I will use raw strings

{- main :: IO ()                                            -}
{- main = do                                                -}
{-     -- putStrLn $ show $ generateI R (MVector MInt) [] -}
{-     putStrLn $ show $ validateE MInt [] (Raw "123")     -}
{-     putStrLn $ show $ validateE MInt [] (Raw "wer")     -}
{-     putStrLn $ show $ validateE MString [] (Raw "wer")  -}
{-     putStrLn $ show $ validateE MNum [] (Raw "wer")     -}
{-     putStrLn $ show $ validateE MNum [] (Raw "1.123")   -}

data ColumnSpec = ColumnSpec {name    :: String       , kind     :: Type} deriving (Show,Read)
data TableSpec  = TableSpec  {columns :: [ColumnSpec] , rownames :: Bool} deriving (Show,Read)
data HashSpec   = HashSpec   {key     :: Type         , value    :: Type} deriving (Show,Read)
data TreeSpec   = TreeSpec   {leaf    :: Type         , node     :: Type} deriving (Show,Read)

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
    | MTable   TableSpec
    | MMatrix  Type
    | MVector  Type
    | MTuple   [Type]
    | MSet     Type
    | MHash    HashSpec
    | MTree    TreeSpec
    deriving (Show, Read)

data Lang
    = R
    | Bash
    | Haskell
    deriving(Show, Read)

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
{-            "Int"    -> return MInt    -}
{-            "String" -> return MString -}
{-            _        -> pfail           -}
{-     )                                  -}
{-   readListPrec = R.readListPrecDefault -}
{-   readList     = R.readListDefault     -}

type Key   = String
type Value = String
type Code  = String
type TypeSpec = [(Lang, Key, Value)]
type EdgeSpec = [(Key, Value)]
data Common = Raw String | Jason String

instance Show Common where
  show (Raw   x) = x
  show (Jason x) = x


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

generateI Bash    _ _ = Nothing
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

validateE MInt    _ (Raw x) =
    case (reads x :: [(Int, String)]) of
        [(a, "")] -> True
        _         -> False

validateE MNum    _ (Raw x) =
    case (reads x :: [(Float, String)]) of
        [(a, "")] -> True
        _         -> False

validateE MChar    _ (Raw x) = length x == 1
validateE MBool    _ (Raw x)
    | x == "true"  = True
    | x == "false" = True
    | otherwise    = False

validateE MVoid   _ _ = True
validateE MString _ _ = True
validateE MText   _ _ = True
validateE MFile   _ _ = True
validateE _       _ _ = False
