rbad <- function() rawToChar(as.raw(c(0x61, 0xff, 0x62, 0x0a)))
rbadvec <- function(n) c("ok\n", rbad())
rbadlist <- function(n) list("ok\n", rbad())
rbadraw <- function(n) list("ok\n", as.raw(c(0x61, 0xff, 0x62, 0x0a)))
rrender <- function(xs) rbad()
rbytes <- function(n) { x <- rbad(); Encoding(x) <- "bytes"; c("ok\n", x) }
rlatin <- function(n) { x <- rbad(); Encoding(x) <- "latin1"; c("ok\n", x) }
