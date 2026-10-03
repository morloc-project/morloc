tick <- function(path) {
  cat("x", file = path, append = TRUE)
  as.integer(file.info(path)$size)
}

ok_ <- function(x) paste("ok", as.integer(x))

h_apply <- function(f, n) ok_(f(n))

h_ops <- function(o, n) ok_(o[["get"]](n))

h_wrap <- function(w, n) ok_(w[["inner"]][["get"]](n))

h_tup <- function(p) ok_(p[[1]](p[[2]]))

h_list <- function(fs, n) ok_(fs[[1]](n) + fs[[2]](n))

h_opt <- function(f, n) if (is.null(f)) "none" else ok_(f(n))

h_map <- function(m, n) ok_(m[["a"]](n))

h_susp_ops <- function(s, n) ok_(s()[["get"]](n))

h_make_ops <- function(k) {
  force(k)
  list(get = function(i) as.integer(i + k))
}

h_make_list <- function(k) {
  force(k)
  list(function(i) as.integer(i + k), function(i) as.integer(i * k))
}

h_cb_ret <- function(mk, n) ok_(mk(n)[["get"]](n))

h_cb_param <- function(cb) ok_(cb(list(get = function(i) as.integer(i * 10))))
