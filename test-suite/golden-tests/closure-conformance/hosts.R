home_tick <- function(path) {
  cat("x", file = path, append = TRUE)
  as.integer(file.info(path)$size)
}
home_ident <- function(x) as.integer(x)
home_mark <- function(s) s
home_str_len <- function(s) nchar(s)
home_add <- function(x, y) as.integer(x + y)
home_fail <- function(path) stop(paste("failHome ran:", path))
home_mk <- function(k) function(x) as.integer(x + k)
home_big <- function(n) as.integer(seq_len(n) - 1L)
home_use_pure <- function(f, x) as.integer(f(x))
home_take_thunk <- function(t) {
  t()
  t()
  as.integer(t())
}

away_ident <- function(x) as.integer(x)
away_bump <- function(path, x) {
  cat("x", file = path, append = TRUE)
  as.integer(file.info(path)$size)
}
away_take_thunk <- function(t) {
  t()
  t()
  as.integer(t())
}
away_drop_thunk <- function(t) 0L
away_use_record <- function(h) {
  h[["run"]]()
  h[["run"]]()
  as.integer(h[["run"]]() + h[["tag"]])
}
away_use_list <- function(xs) {
  xs[[1]]()
  xs[[1]]()
  as.integer(xs[[1]]())
}
away_use_tuple <- function(t) {
  t[[1]]()
  t[[1]]()
  as.integer(t[[1]]() + t[[2]])
}
away_use_opt <- function(t) {
  if (is.null(t)) return(0L)
  t()
  t()
  as.integer(t())
}
away_use_nested <- function(hs) {
  hs[[1]][["run"]]()
  hs[[1]][["run"]]()
  as.integer(hs[[1]][["run"]]())
}
away_use_callback <- function(f, x) {
  f(x)
  f(x)
  as.integer(f(x))
}
away_use_pure <- function(f, x) as.integer(f(x))
away_use_arity2 <- function(f, x, y) as.integer(f(x, y))
away_use_pinned <- function(c, x) {
  c[["inc"]](x)
  c[["inc"]](x)
  as.integer(c[["inc"]](x))
}
away_use_hof <- function(f, g) as.integer(f(g))
away_use_list_pure <- function(fs, x) {
  s <- 0L
  for (f in fs) s <- s + f(x)
  as.integer(s)
}
away_use_mk <- function(f, a, b) as.integer(f(a)(b))
away_use_thunk2 <- function(t) {
  inner <- t()
  inner()
  inner()
  as.integer(inner())
}
away_use_thunk_fn <- function(t, x) as.integer(t()(x))
away_use_fn_thunk <- function(f, t) as.integer(f(t))
away_pass_through <- function(f) f
away_map_away <- function(f, xs) as.integer(vapply(xs, f, integer(1)))

away_use_hof_eff <- function(f, g) as.integer(f(g))

away_use_hof2 <- function(f) as.integer(f(function(g) as.integer(g(3))))

away_two <- function(a, b, x) {
  cat("x", file = a, append = TRUE)
  as.integer(file.info(a)$size + nchar(b))
}
away_mix <- function(x, a) {
  cat("x", file = a, append = TRUE)
  as.integer(file.info(a)$size + x)
}
