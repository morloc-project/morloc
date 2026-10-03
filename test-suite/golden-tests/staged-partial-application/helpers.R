tick <- function(x) {
  cat("tick\n", file = Sys.getenv("TICK_LOG"), append = TRUE)
  x
}

count_ticks <- function() {
  f <- Sys.getenv("TICK_LOG")
  if (file.exists(f)) length(readLines(f)) else 0L
}

ticks <- function(x) {
  force(x)
  count_ticks()
}

ticks_l <- function(x) {
  force(x)
  count_ticks()
}

host_map <- function(f, xs) sapply(xs, f)

host_map2 <- function(f, xs, ys) mapply(f, xs, ys)
