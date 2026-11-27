rbad <- function(n) rawToChar(as.raw(c(0x61, 0x62, 0xff, 0x63)))
rlatin <- function(n) { x <- rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xe9))); Encoding(x) <- "latin1"; x }
rbytes <- function(n) { x <- rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xc3, 0xa9))); Encoding(x) <- "bytes"; x }
reuro <- function(n) { x <- rawToChar(as.raw(0x80)); Encoding(x) <- "latin1"; x }
rc1 <- function(n) { x <- rawToChar(as.raw(c(0x41, 0x90))); Encoding(x) <- "latin1"; x }
