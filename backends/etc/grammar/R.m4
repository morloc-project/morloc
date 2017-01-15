m4_define(`PROLOGUE',
    `#!/usr/bin/Rscript --vanilla'
    `library(readr)'
)

m4_define(`NATIVE_MANIFOLD',
    $1 <- function(){
        CACHE_$1
        RETURN
    }
)

m4_define(`UNIVERSAL_MANIFOLD',
    $1 <- function(){
        CACHE_$1
        RETURN
    }
)

m4_define(`FOREIGN_MANIFOLD',
    $2 <- function(){
        d <- system("./call.$1 $2", intern = TRUE)
        READ(TYPE_$2)
        d <- read_tsv(d)
        if(ncol(d) == 1){
            d <- d[[1]]
        }
        d
    }
)

m4_define(`READ', )

m4_define(`RETURN', `Mb')


m4_define(`DO_CACHE',
    if(BASECACHE_$1``_chk''($1)){
        BASECACHE_$1``_get''($1)
    } else {
        VALIDATE_$1
    }
)

m4_define(`NO_CACHE',
    VALIDATE_$1
    HOOK_$1
)


m4_define(`DO_VALIDATE',
`
    if(CHECK_$1){
        CORE($1)
    } else {
        Mb <- FAIL_$1`()'
        m4_dnl missing cache
    }
'
)

m4_define(`NO_VALIDATE', `CORE($1)')

m4_define(`CHECK', $1`()')

m4_define(`CORE',
`
    b <- RUN_$1
    EFFECT_$1
    PACK_$1
    CACHE_PUT_$1
'
)

m4_define(`DO_CACHE_PUT', BASECACHE_$1`("$1")')

m4_define(`NO_CACHE_PUT', )

m4_define(`DO_PACK', Mb <- PACKFUN_$1`(b)')

m4_define(`NO_PACK', Mb <- b)

m4_define(`DO_PASS', `PASS_$1`(FUNC_$1 ARG_$1 INPUT_$1)'')

m4_define(`NO_PASS', `FUNC_$1`(ARG_$1 INPUT_$1)'')

m4_define(`CALL', $1`()')

m4_define(`EFFECT', `$1(b)\n')

m4_define(`HOOK', `$1()\n')

m4_define(`NOTHING', `NULL')

m4_define(`SIMPLE_FAIL', `return NULL')

m4_define(`NO_PUT', )

m4_define(`DO_PUT', `BASECACHE_$1`_put'($1)')

m4_define(`EPILOGUE',

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

)
