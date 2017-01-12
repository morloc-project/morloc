#!/usr/bin/env bash
set -u

usage (){
cat << EOF
Test the final output of synder
OPTIONAL ARGUMENTS
  -h  print this help message
  -q  quiet, print no output
EOF
    exit 0
}

quiet=0
while getopts "hdqxvma:o:" opt; do
    case $opt in
        h)
            usage ;;
        q)
            quiet=1 ;;
        ?)
            warn "Illegal arguments"
            exit 1
    esac 
done

n_fail=0
n_pass=0
loc=~/.loc/bin/loc

announce(){
    if [[ $quiet -eq 0 ]]
    then
        [[ -t 1 ]] && o="\e[1;33m$1\e[0m" || o=$1
        echo -e $o
    fi
}

warn(){
    if [[ $quiet -eq 0 ]]
    then
        [[ -t 1 ]] && o="\e[1;31m$1\e[0m" || o=$1
        echo -en $o
    fi
}

emphasize_n(){
    if [[ $quiet -eq 0 ]]
    then
        [[ -t 1 ]] && o="\e[1;39m$1\e[0m" || o=$1
        echo -ne $o
    fi
}

emphasize(){
    if [[ $quiet -eq 0 ]]
    then
        emphasize_n "$1" 
        echo
    fi
}

say_n(){
    [[ $quiet -eq 0 ]] && echo -n "$@"
}

say(){
    [[ $quiet -eq 0 ]] && echo "$@"
}

runtest(){
    dir=$1
    msg=$2
    cd $dir

    echo -n "$msg ... "

    $loc x.loc 2>&1 > /dev/null
    if [[ $? == 0 ]]
    then
        diff <($loc x.loc) x.lil > /dev/null
        if [[ $? == 0 ]]
        then
            echo OK
            n_pass=$(( n_pass + 1 ))
        else
            warn FAIL
            echo " - bad LIL"
            n_fail=$(( n_fail + 1 ))
        fi
    else
        warn FAIL
        echo " - non-zero exit status"
        n_fail=$(( n_fail + 1 ))
    fi
    cd - > /dev/null
}


runtest elements "Manifold elements exist"
runtest import "Basic import"
runtest list-select "Lists expand on couplet lhs"


echo
if [[ $n_fail == 0 ]]
then
    echo All tests pass
else
    warn "FAILURE"
    echo " - $n_fail out of $(( n_fail + n_pass )) tests failed"
fi
