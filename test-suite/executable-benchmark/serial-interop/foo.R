rid <- function(x) x

# Generate a 1 MB random string
nmb <- function(n){
  stringi::stri_dup("x", n * 1024 * 1024)
}
