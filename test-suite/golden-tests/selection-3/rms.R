rms1 <- function(xs){
  sqrt ( sum(xs ^ 2) / length(xs) )
}

rms2 <- function(xs){
  sqrt (mean (xs ^ 2))
}

add <- function(x, y){
  x + y
}
