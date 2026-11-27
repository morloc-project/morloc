# The native form is a list; the wire form is an integer. A handler given
# the wire form fails on `$`.

pack_tally <- function(x) list(v = x)

unpack_tally <- function(t) t$v

produce <- function(sink) {
  sink(list(pack_tally(1L), pack_tally(2L)))
  sink(list())
  sink(list(pack_tally(3L)))
}

show_all <- function(xs) {
  paste0("r ", paste(vapply(xs, function(t) as.character(t$v), ""), collapse = " "), "\n")
}

show_batch <- function(xs) {
  paste0("r batch ", length(vapply(xs, function(t) t$v, 0L)), "\n")
}
