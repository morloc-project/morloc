r_read_opt <- function(x) {
    if (x < 0L) return(NULL)
    return(as.integer(x))
}

r_add_one <- function(x) {
    return(as.integer(x) + 1L)
}

r_apply_thunk <- function(f) {
    return(as.integer(f()) + 1L)
}

r_sum_thunks <- function(xs) {
    s <- 0L
    for (f in xs) {
        s <- s + as.integer(f())
    }
    return(s)
}
