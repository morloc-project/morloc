apply2 <- function(f, x) f(x) + f(x)

tick <- function(x) {
  cat("tick\n", file = Sys.getenv("TICK_LOG"), append = TRUE)
  x
}

ticks <- function(x) {
  force(x)
  f <- Sys.getenv("TICK_LOG")
  if (file.exists(f)) length(readLines(f)) else 0L
}
