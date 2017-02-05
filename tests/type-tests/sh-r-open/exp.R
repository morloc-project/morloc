#!/usr/bin/Rscript --vanilla

#types: Numeric, String, Vector, Table, Filename, Binary

types = c(m0="Int", m1="String", x1="Int")

m0 <- function(x1, uid){
  u1 <- native_to_universal(x1, types["x1"])
  u2 <- native_to_universal(uid, "String")
  d <- system(sprintf("a/call.sh m0 %s %s", u1, u2), intern = TRUE)
  d <- universal_to_native(d, types["m0"])
  return(d)
}

m1 = function (){
  seq(from=1, to=100)
}

args <- commandArgs(TRUE)
m <- args[1]

if(exists(m)){
  f = get(m)
  if(length(args[-1]) == 0){
    d <- f()
  } else {
    d <- f(args[-1])
  }
  dat = native_to_universal(d, types[m])
  write_file(dat, file=stdout)
} else {
  quit(status=1)
}
