orphan <- function(seconds) {
  parent <- as.integer(system(paste("ps -o ppid= -p", Sys.getpid()), intern = TRUE))
  tools::pskill(parent, tools::SIGKILL)
  Sys.sleep(seconds)
  seconds
}
