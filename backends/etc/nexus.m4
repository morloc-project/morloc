m4_changequote(`<[', `]>')

m4_define(<[__MANIFOLD__]>,
<[MANGLE(ALIAS_$1) () {
    MANIFOLD_POOL($1) $1 
}]>
)

m4_define(<[MANGLE]>, __manifold__$1)

m4_define(<[MANIFOLD_POOL]>, ./call.LANG_$1 )

m4_define(<[__MANIFOLD_DOC__]>, <[ALIAS_$1   LANG_$1   MDOC_$1]>)
