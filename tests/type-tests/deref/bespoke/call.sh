#!/usr/bin/env bash

outdir=/home/shoggoth/src/git/loc/tests/type-tests/deref/bespoke

m0 () {
    echo $(wrap_m1 1)
}

m1_uid=0
wrap_m1 () {
    m1_uid=$(( m1_uid + 1 ))
    m1 $@
}

m1 () {
    read_m1 <($outdir/call.R m1 $1 $m1_uid)
}

show_m0 (){
    echo "$(m0)"
}

read_m1 (){
    echo "$(cat $1)"
}


manifold_exists() {
    type $1 | grep -q function
}
if manifold_exists $1
then
    show_$1
else
    exit 1
fi
