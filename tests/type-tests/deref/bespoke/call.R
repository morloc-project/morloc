#!/usr/bin/Rscript --vanilla
library(readr)
suppressPackageStartupMessages(library("jsonlite"))

outdir <- "/home/shoggoth/src/git/loc/tests/type-tests/deref/bespoke"

m1 = function (x1, uid){
  x1
}

show_m1 <- function(...){
    toJSON(m1(...), auto_unbox=TRUE, null="null")
}

read_m1 <- function(x){
    fromJSON(x, simplifyVector=TRUE)
}


args <- commandArgs(TRUE)
m <- args[1]

if(exists(m)){
  cmd = paste0("show_", m)
  f <- get(cmd)
  d <- do.call(f, as.list(args[-1]))
  write(d, file=stdout())
} else {
  quit(status=1)
}
