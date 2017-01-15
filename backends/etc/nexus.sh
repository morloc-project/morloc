#!/usr/bin/env bash

usage (){
cat << EOF
DESC
    Manifold nexus

USAGE
    __NAME__ [options] MANIFOLD

REQUIRED ARGUMENTS
    -h print this help message and exit

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
