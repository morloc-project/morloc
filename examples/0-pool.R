#!/usr/bin/env Rscript

source("r-base.R")

# transpose :: Matrix a -> Matrix a
m46 <- function (x0){
  t(unpackMatrix(x0))
}

# nrow :: Table -> Int;
m56 <- function (x0){
  nrow(x0)
}

# sum :: [Num] -> Num;
m76 <- function (x0){
  sum(unpackNumeric(x0))
}

# rand_uniform :: n:Int, a:Num, b:Num -> xs:[c:Num] where (
#     n > 0
#   , len xs == n
#   , c >= a
#   , c <= b
# );
m13 <- function (x0, x1, x2){
  runif(unpackNumeric(x0), unpackNumeric(x1), unpackNumeric(x2))
}

# ceiling :: [Num] -> [Int];
m40 <- function (x0){
  ceiling(unpackNumeric(x0))
}

args <- commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  stop("Expected 1 or more arguments")
} else if(exists(args[[1]])){
  x <- get(args[[1]])
  result <- if(class(x) == "function"){
    do.call(get(args[[1]]), as.list(args[-1, drop=FALSE]))
  } else {
    x
  }
  cat(packGeneric(result))
} else {
  stop("Could not find function '", args[[1]], "'")
}
