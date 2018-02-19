# _ :: Num -> Num
f <- function(x) x + 1

# Set bounds on the indexing variable
# `[*]` indicate a vector of any type
# _ :: [*] -> Int 1 _ -> [*]
f <- function(x, i) x[i] 

# numeric vector with no NA
# _ :: [Num] -> [Num]
f <- function(x) x + 1

# numeric vector with NA
# _ :: [?Num] -> [?Num]
f <- function(x) x + 1

# _ :: Matrix Num -> [Num]
# _ :: Table -> [Num]
f <- function(x){
  colSums(x)
}


## Two different language specific specializations of Table
#-- RDataFrame ::
#--   lang "R"
#--   cast_as "data.frame"
#--   is_a Table
#-- RDataTable ::
#--   lang "R"
#--   cast_as "data.table"
#--   import "data.table"
#--   is_a Table

## Define the casting function
#-- as.data.table :: role "cast" => RDataFrame a -> RDataTable a
#-- as.data.frame :: role "cast" => RDataTable a -> RDataFrame a

## Finite set of symbols (Enum) but encoded in R as a string (rather than the
## default, a factor). We avoid the issues of handling factors, but retain the
## semantic information.
#-- RStringFactor ::
#--   lang "R"
#--   cast_as "character"
#--   is_a Enum

#-- foo :: RDataFrame {a:Integer; b:String; c:RStringFactor (Cat, Dog, Eel)} ->
#--        RDataFrame {a:Integer; b:String}
foo <- function(x){
  subset(x, c == "Eel")[-3]
}
