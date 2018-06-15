module Morloc (parseMorloc) where

import Morloc.Data
import Morloc.Parser (morlocScript)
import Text.Parsec (parse, SourceName)

parseMorloc :: SourceName -> String -> String
parseMorloc file s =
  case parse morlocScript file s of
    Left  err    -> show      err    ++ "\n"
    Right result -> showMorloc result ++ "\n"

-- | a somewhat pretty printer, it doesn't convert all the way back to the
-- input code, although perhaps that would be a reasonable thing to do. 
showMorloc :: [Top] -> String
showMorloc = (unlines . map showMorloc') where
  showMorloc' :: Top -> String
  showMorloc' (TopImport (Import path qual rest)) =
    unwords ["from", show path, "import", show rest, "as", show qual]
  showMorloc' (TopStatement (Signature name ins out constraints)) =
    unwords [name, "::", show ins, "->", show out, "where", show constraints]  
  showMorloc' (TopSource x) = ""
  showMorloc' x = show x
