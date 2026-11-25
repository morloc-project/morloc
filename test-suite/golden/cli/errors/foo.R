rid <- function(x) x

rdiv <- function(x){
    if(x == 0){
        stop("Cannot divide by zero")
    } else {
        1 / x
    }
}
