sideEffectR <- function(x) {
    cat(paste0("EVAL_R ", x, "\n"))
    flush(stdout())
    x * 2L
}

addR <- function(a, b) {
    a + b
}
