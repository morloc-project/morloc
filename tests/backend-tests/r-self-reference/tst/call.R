#!/usr/bin/Rscript --vanilla
library(readr)
require(readr)
datcache_path <- function(mid) {
file.path("cache", paste0(mid, ".Rdat"))
}
# String -> Bool
datcache_chk <- function(mid) {
file.exists(datcache_path(mid))
}
# NULL -> Text
datcache_get <- function(mid) {
read_rds(datcache_path(mid))
}
# Text -> Text
datcache_put <- function(mid, dat) {
write_rds(dat, datcache_path(mid))
}
null <- function(...) { NULL }
pdf_hist <- function(x, path){
pdf(path)
hist(x)
dev.off()
}
m0 <- function(){
b <- null ( m1 ())
Mb <- b
Mb
}
m1 <- function(){
if(datcache_chk("m1")){
Mb = datcache_get("m1")
} else {
b <- runif ( 100)
Mb <- b
datcache_put("m1", Mb)
m2 ()
}
Mb
}
m2 <- function(){
b <- pdf_hist ( m1 (),  "z.pdf")
Mb <- b
Mb
}
args <- commandArgs(TRUE)
m <- args[1]
if(exists(m)){
f = get(m)
d <- f()
if(is.data.frame(d)){
write_tsv(d, path="/dev/stdout")
} else {
write_lines(d, path="/dev/stdout")
}
} else {
quit(status=1)
}
