price <- function(t) {
  col <- t$column(1)
  paste(format(as.vector(col), nsmall = 2), col$type$ToString())
}
