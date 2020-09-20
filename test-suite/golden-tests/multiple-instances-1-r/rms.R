# the smart way
rms1 <- function(xs){
  sqrt(mean(xs^2))
}

# the dumb way
rms2 <- function(xs){
  result = 0
  for (x in xs){
    result = result + x ^ 2
  }
  result = result / length(result)
  result = sqrt(result)
  return(result)
}
