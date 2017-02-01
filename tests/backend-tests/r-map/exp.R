#!/usr/bin/Rscript --vanilla
library(readr)

# outdir=a

map <- function(f, x){
    lapply(x, f)
}

m0 = function (){
  b = map (wrap_m1, m2())
  return(b)
}

m1_uid = 0
wrap_m1 <- function(x0){
  m1_uid <<- m1_uid + 1
  uid <- m1_uid
  m1(x0, uid)
}
m1 = function (x0, uid){
  b = sqrt(x0)
  return(b)
}

m2 = function (){
  b = c(1, 4, 9, 16)
  return(b)
}

args <- commandArgs(TRUE)
m <- args[1]

if(exists(m)){
  f = get(m)
  d <- f()
  if(is.data.frame(d)){
    write_tsv(d, path="/dev/stdout")
  } else {
    write_lines(d, path="/dev/stdout")
  }
} else {
  quit(status=1)
}
