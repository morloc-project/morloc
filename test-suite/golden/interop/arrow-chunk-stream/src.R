nRows <- function(t) t$num_rows

sumOver <- function(f, xs) {
  total <- 0L
  for (x in xs) total <- total + f(x)
  total
}
