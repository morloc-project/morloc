#!/bin/awk -f 

BEGIN {

    FS="\t"

    printf("m4_define(`__NAME__', %s)\n", name)
    printf("m4_define(`OUTDIR', %s)\n", dir)

}

$1 == "EMIT" { m[$2] = $3 }
$1 == "FUNC" { printf("m4_define(`FUNCTION_%s', %s)\n", $2, $3) }

END {
    manifold=""
    manifold_doc=""
    for(k in m){
       printf("m4_define(`LANG_%s', %s)", k, m[k])
       manifold = sprintf("%s__MANIFOLD__(%s)\n", manifold, k)
       manifold_doc = sprintf("%s    __MANIFOLD_DOC__(%s)\n", manifold_doc, k)
    }
    printf("m4_define(`__MANIFOLD_WRAPPERS__', %s)", manifold)
    printf("m4_define(`__MANIFOLD_DOCUMENTATION__', %s)", manifold_doc)
}
