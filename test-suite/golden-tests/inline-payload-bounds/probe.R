u64_bytes <- function(x) {
  as.raw(sapply(0:7, function(i) (x %/% 256^i) %% 256))
}

probe <- function(size, data) {
  size <- as.numeric(size)
  data <- as.numeric(data)
  p <- morloc_put_value(c(1L, 2L, 3L), "au1")
  if (as.integer(p[14]) != 0) return("not inline")
  off <- sum(as.integer(p[21:24]) * 256^(0:3))
  base <- 32 + off
  if (size >= 0) p[(base + 1):(base + 8)] <- u64_bytes(size)
  if (data >= 0) p[(base + 9):(base + 16)] <- u64_bytes(data)
  tryCatch(
    paste("ok", length(morloc_get_value(p, "au1"))),
    error = function(e) "refused"
  )
}
