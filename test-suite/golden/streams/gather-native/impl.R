produce_forks <- function(sink) {
  sink(0L)
  parallel::mclapply(1:2, function(c) { sink(c(10L * c + 1L)); 0L }, mc.cores = 2L)
  sink(99L)
}
add_batch <- function(acc, xs) acc + length(xs)
merge <- function(a, b) a + b
show_n <- function(n) paste0("r n=", n, "\n")
