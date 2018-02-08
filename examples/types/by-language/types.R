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
