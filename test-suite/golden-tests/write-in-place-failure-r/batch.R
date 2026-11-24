batch_of <- function(b, n, bad) {
  xs <- paste0(b, "-", seq_len(n) - 1L, strrep("x", 40))
  if (bad >= 0 && bad < n) xs[bad + 1L] <- NA
  xs
}
batch_list <- function(b, n, bad) as.list(batch_of(b, n, bad))
count_tagged <- function(b, xs) sum(startsWith(xs, paste0(b, "-")))
i8_batch <- function(n, bad) {
  xs <- as.list((seq_len(n) - 1L) %% 100L)
  if (bad >= 0 && bad < n) xs[[bad + 1L]] <- 300L
  xs
}
raw_of <- function(b, n, bad) {
  xs <- lapply(seq_len(n) - 1L, function(i) charToRaw(paste0(b, "-", i, strrep("x", 40))))
  if (bad >= 0 && bad < n) xs[[bad + 1L]] <- as.raw(c(0x61, 0xff, 0x62))
  xs
}
