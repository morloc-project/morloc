r_fold <- function(f, init, xs) {
  acc <- init
  for (x in xs) acc <- f(acc, x)
  acc
}

r_tick <- function(s) invisible(NULL)
