crash <- function(x) {
  tools::pskill(Sys.getpid(), 11L)
  x
}
