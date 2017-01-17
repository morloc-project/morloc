#!/usr/bin/env bash

usage (){
cat << EOF
DESC
    Manifold nexus

USAGE
    manifold-nexus.sh [options] MANIFOLD

REQUIRED ARGUMENTS
    -h print this help message and exit

MANIFOLDS
    null
    runif
    pdf_hist

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



__manifold__null () {
    ./call.*  m0 
}

__manifold__runif () {
    ./call.R  m1 
}

__manifold__pdf_hist () {
    ./call.R  m2 
}





function_exists() {
    type $1 | grep -q function
}

if function_exists __manifold__$1
then
    cd /home/rnz/src/git/loc/tests/backend-tests/r-self-reference/tst
    __manifold__$1
else
    exit 1
fi
