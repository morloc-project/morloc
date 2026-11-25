r_io <- function(x) {
    cat(paste0("r:", x, "\n"), file = stderr())
    x + 300L
}

r_gate <- function(x) {
    x > 0L
}
