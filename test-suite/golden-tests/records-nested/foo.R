addBar <- function(bar, foo){
    foo$bars[[length(foo$bars) + 1]] <- bar
    foo
}
