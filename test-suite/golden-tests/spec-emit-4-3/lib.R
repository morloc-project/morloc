step <- function(k, i) {
  if (i > k) stop("an operand after the deciding one ran")
  i != k
}

hit <- function(k, i) {
  if (i > k) stop("an operand after the deciding one ran")
  i == k
}
