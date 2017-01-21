require(readr)

datcache_path <- function(mid, uid=null) {
    if(is.null(uid)){
        file.path("cache", paste0(mid, ".rdat"))
    } else {
        file.path("cache", paste0(mid, "_", uid, ".rdat"))
    }
}

# string -> bool
datcache_chk <- function(mid, uid=null) {
    file.exists(datcache_path(mid, uid))
}

# null -> text
datcache_get <- function(mid, uid=null) {
    read_rds(datcache_path(mid, uid))
}

# text -> text
datcache_put <- function(mid, dat, uid=null) {
    write_rds(dat, datcache_path(mid, uid))
}

datcache_del <- function(mid, uid=null) {
    file.remove(datcache_path(mid, uid))
}

# Load a builtin R dataset and return
load_data <- function(x) {
  eval(parse(text=x))
}

# Normalize numeric columns
normalize <- function(x) {
  as.data.frame(lapply(x, function(x) (x - mean(x)) / sd(x)))
}

columns_are_numeric <- function(x) {
  all(sapply(x, is.numeric)) 
}

plot_pdf <- function(x, name){
    path <- paste0("f-", name, ".pdf")
    pdf(path)
    plot(x)
    dev.off()
}

map <- function(f, x){
    lapply(x, f)
}



doit_uid <- 0

doit <- function() {
    map(wrap_m2, m7())
}

wrap_m2 <- function(x){
    doit_uid <<- doit_uid + 1
    m2(x, doit_uid)
}

m2 <- function(x, uid){
    if(datcache_chk("m2", uid)){
        b <- datcache_get("m2", uid)
    } else {
        b <- hclust( m3(x, uid) )
        datcache_put("m2", b, uid)
        m6(x, uid)
    }
    b
}

m3 <- function(x, uid){
    dist(m4(x, uid))
}

m4 <- function(x, uid){
    if(m8(x, uid)){
        normalize( m5(x, uid) )
    } else {
        warning("check failed")
        NULL
    }
}

m5 <- function(x, uid){
    load_data(x)
}

m6 <- function(x, uid){
    plot_pdf(m2(x, uid), x)
}

m7 <- function(){
    c("cars", "mtcars")
}

m8 <- function(x, uid){
    columns_are_numeric(m5(x, uid))
}
