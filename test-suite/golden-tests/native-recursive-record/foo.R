sum_tree <- function(t) {
    total <- t$value
    for (c in t$children) {
        total <- total + sum_tree(c)
    }
    total
}

ll_len <- function(node) {
    n <- 0L
    while (!is.null(node)) {
        n <- n + 1L
        node <- node$tail
    }
    n
}
