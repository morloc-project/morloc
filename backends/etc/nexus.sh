#!/usr/bin/env bash

usage (){
cat << EOF
DESC
    Manifold nexus

USAGE
    __NAME__ [options] MANIFOLD

ARGUMENTS
    -h print this help message and exit
    -x remove all loc_* directories

MANIFOLDS
    __MANIFOLD_DOCUMENTATION__
EOF
    exit 0
}

[[ $# -eq 0 ]] && usage

while getopts "h" opt; do
    case $opt in
        h)
            usage ;;
        x)
            rm -rf loc_* /tmp/loc_*
            exit 0 ;;
    esac 
done



__MANIFOLD_WRAPPERS__



function_exists() {
    type $1 | grep -q function
}

if function_exists MANGLE($1)
then
    cd OUTDIR
    MANGLE($1)
else
    exit 1
fi
