define(__MANIFOLD__,
`MANGLE(FUNCTION_$1) () {
    MANIFOLD_POOL($1) $1 
}'
)

define(MANGLE, __manifold__$1)

define(MANIFOLD_POOL, OUTDIR/call.LANG_$1 )

define(__MANIFOLD_DOC__, FUNCTION_$1(LANG_$1))
