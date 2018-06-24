#!/usr/bin/env Rscript

m1 <- function(n, size){
  ceiling(m2(n, size))
}

m2 <- function(n, size){
  runif(n, m4, size)
}

m3 <- function(n, size){
  n
}

m4 <- 0L
m5 <- function(n, size){
  size
}

m7 <- function(n){
  runif(n, m9, m10)
}

m8 <- function(n){
  n
}

m9 <- 0L
m10 <- 1L
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
