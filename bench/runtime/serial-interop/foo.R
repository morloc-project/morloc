rid <- function(x) x

# Generate an n MB random string
nmb <- function(n){
  stringi::stri_dup("x", n * 1024 * 1024)
}

incr <- function(x, y) x + y

addr <- function(x, y) x + y
