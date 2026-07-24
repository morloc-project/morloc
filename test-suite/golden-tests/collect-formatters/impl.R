make_ints <- function(i) {
  c(i * 10 + 3, i * 10 + 1, i * 10 + 2)
}

sort_ints <- function(xs) {
  sort(xs)
}

tag_ints <- function(xs) {
  c(xs, length(xs))
}

tag_ints_off <- function(off, xs) {
  c(off, xs)
}

render_ints <- function(xs) {
  paste0(paste(xs, collapse = " "), " \n")
}

render_ints_off <- function(off, xs) {
  paste0("[", off, "]", paste0(" ", xs, collapse = ""), "\n")
}

make_strs <- function(i) {
  c(paste0("b", i), paste0("a", i))
}

sort_strs <- function(xs) {
  sort(xs)
}

join_strs <- function(xs) {
  paste0(paste(xs, collapse = ";"), ";\n")
}
