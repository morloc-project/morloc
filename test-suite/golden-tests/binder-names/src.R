main <- function(x) x
args <- 1L
status <- 2L
tmpdir <- "user"
socket_path <- "user"
shm_basename <- "user"
global_state <- list(k = 3L)
worker_init <- function() stop("user worker_init called")
mlc_encode <- function(x) x * 10L
morloc_get_value <- function(...) stop("user morloc_get_value called")
morloc_put_value <- function(...) stop("user morloc_put_value called")
morloc_mlc_pipe_closed <- function(...) stop("user morloc_mlc_pipe_closed called")
.mlc_entry <- function(...) stop("user .mlc_entry called")
.mlc_wrap_log <- function(...) stop("user .mlc_wrap_log called")
.dispatch <- list()
mlc_schema_table <- list()
mlc_stage_table <- list()
mlc_closure_table <- list()
MLC_HOME_LANG <- "user"
MLC_CLOSURE_SCHEMA <- "user"
get <- function(...) stop("user get called")

.morloc.source("helper.R")

conditionMessage <- function(c) "HIJACK"
conditionMessage.user_condition <- function(c) paste("classed", c$detail)

setClass("Box", representation(x = "numeric"))

r_ap <- function(f, x) f(x)
r_box <- function(x) { b <- new("Box", x = x); as.integer(b@x) + 1L }
r_twice <- function(x) r_helper(x)
r_fail <- function(x) stop("boom")
r_fail_classed <- function(x) stop(structure(class = c("user_condition", "error", "condition"), list(message = "plain", call = NULL, detail = "boom")))
r_add <- function(a, b) a + b

r_count <- function(n) {
  main(n) + args + status + nchar(tmpdir) + global_state$k + mlc_encode(0L)
}
