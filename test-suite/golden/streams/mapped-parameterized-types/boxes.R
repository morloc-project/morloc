# As in Python: the mapping is a hint, the value is structural.
grow <- function(b) {
    if (b[[1]] == "Full") list("Full", list(b[[2]][[1]] + 1000)) else b
}

bump <- function(w) {
    w$item <- w$item + 1000
    w
}
