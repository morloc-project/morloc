rOk <- function(x) x

rDie <- function(x) {
  tools::pskill(Sys.getpid(), 9L)
  x
}

rSleep <- function(x) {
  Sys.sleep(2)
  x
}
