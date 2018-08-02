#!/usr/bin/env Rscript
m4 <- function (x1, x2, x3){
  runif(x1, x2, x3)
}

m5 <- function (x1){
  ceiling(x1)
}

m18 <- function (){
  len(xs)
}

m44 <- function (n){
  rand_uniform(n, 0.0, 1.0)
}

args <- commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  stop("Expected 1 or more arguments")
} else if(exists(args[[1]])){
  x <- get(args[[1]])
  result <- if(class(x) == "function"){
    par <- lapply(args[-1], function(s) eval(parse(text=s)))
    do.call(get(args[[1]]), par)
  } else {
    x
  }
  cat(result, "\n")
} else {
  stop("Could not find function '", f, "'")
}
