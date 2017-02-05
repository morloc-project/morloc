#!/usr/bin/env bash

outdir=locout



to_stderr () {
    $@ 1>&2
}

m0 () {
    locout/call.R m0
}

m1_uid=0
wrap_m1 () {
    m1_uid=$(( m1_uid + 1 ))
    m1 $@ $m1_uid
}


m1 () {
    locout/call.R m1 $1 $2
}

m2 () {
    seq  1 $1
}

m3 () {
    locout/call.R m3
}

manifold_exists() {
    type $1 | grep -q function
}
if manifold_exists $1
then
    if [[ -f $outdir/$1_tmp ]]
    then
        cat $outdir/$1_tmp
    else
        $@
    fi
else
    exit 1 
fi

