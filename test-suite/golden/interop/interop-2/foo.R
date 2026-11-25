morloc_fold <- function(f, b, xs){
  for(x in xs){
    b <- f(b, x)
  }
  b
}

morloc_add <- function(x, y){
  x + y
}
