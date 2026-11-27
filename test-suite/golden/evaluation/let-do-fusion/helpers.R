# Side-effecting functions: write a tag to stderr, return a deterministic value.
r_echo <- function(x) {
  cat("R", x, "\n", sep="", file=stderr())
  flush(stderr())
  x + 1L
}

r_add <- function(a, b) a + b

r_mul <- function(a, b) a * b

r_pure <- function(x) x + 10L

r_id <- function(x) x

.r_counter <- 0L
r_next_n <- function() {
  .r_counter <<- .r_counter + 1L
  .r_counter
}
