-- Eventually I will need to use JSON, for that I can use the Aeson library:
-- http://hackage.haskell.org/package/aeson-1.2.0.0/docs/Data-Aeson.html
-- But for now I will use raw strings

main :: IO ()
main = do
    -- putStrLn $ show $ generateI R (M_Vector M_Int) []
    putStrLn $ show $ validateE M_Int [] (Raw "123")
    putStrLn $ show $ validateE M_Int [] (Raw "wer")
    putStrLn $ show $ validateE M_String [] (Raw "wer")
    putStrLn $ show $ validateE M_Num [] (Raw "wer")
    putStrLn $ show $ validateE M_Num [] (Raw "1.123")

data ColumnSpec = ColumnSpec {name::String, kind::Type} deriving (Show)
data TableSpec = TableSpec {columns::[ColumnSpec], rownames::Bool} deriving (Show)
data HashSpec = HashSpec {key::Type, value::Type} deriving (Show)
data TreeSpec = TreeSpec {leaf::Type, node::Type} deriving (Show)

data Type 
    = M_Void
    | M_Int
    | M_Num
    | M_Char
    | M_String
    | M_Bool
    | M_Text
    | M_File
    | M_Binary
    | M_Table   TableSpec
    | M_Matrix  Type
    | M_Vector  Type
    | M_Tuple   [Type]
    | M_Set     Type
    | M_Hash    HashSpec
    | M_Tree    TreeSpec
    deriving (Show)

data Lang
    = R
    | Bash
    | Haskell
    deriving(Show)

type Key   = String
type Value = String
type Code  = String
type TypeSpec = [(Lang, Key, Value)]
type EdgeSpec = [(Key, Value)]
data Common = Raw String | Jason String


-- ========================================================================= --
--------------------------- C O N V E R T E R S -------------------------------
-- ========================================================================= --
convertE  :: Type -> Type -> EdgeSpec -> Common -> Maybe Common
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

validateE M_Int    _ (Raw x) =
    case ((reads x) :: [(Int, String)]) of
        [(a, "")] -> True
        _         -> False

validateE M_Num    _ (Raw x) =
    case ((reads x) :: [(Float, String)]) of
        [(a, "")] -> True
        _         -> False

validateE M_Char   _ (Raw x) = length x == 1
validateE M_Bool   _ (Raw x)
    | x == "true"  = True
    | x == "false" = True
    | otherwise    = False

validateE M_Void   _ _ = True
validateE M_String _ _ = True
validateE M_Text   _ _ = True
validateE M_File   _ _ = True
validateE _        _ _ = False
