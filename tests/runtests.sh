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

warn(){
    if [[ $quiet -eq 0 ]]
    then
        [[ -t 1 ]] && o="\e[1;31m$1\e[0m" || o=$1
        echo -en $o
    fi
}

frontend_test(){
    dir=$1
    msg=$2
    cd frontend-tests/$dir

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

backend_test(){
    dir=$1
    cmd=$2
    msg=$3

    cd backend-tests/$dir
    
    echo -n "$msg ... "

    loc -o tst x.loc 2>&1 > /dev/null
    if [[ $? == 0 ]]
    then
        diff <(tst/manifold-nexus.sh $cmd) <(./x) > /dev/null
        if [[ $? == 0 ]]
        then
            echo OK
            n_pass=$(( n_pass + 1 ))
        else
            warn FAIL
            echo " - unexpected output"
            n_fail=$(( n_fail + 1 ))
        fi
    else
        warn FAIL
        echo " - non-zero exit status"
        n_fail=$(( n_fail + 1 ))
    fi
    
    rm -rf tst
    cd - > /dev/null
}

frontend_test elements "Manifold elements exist"
frontend_test import "Basic import"
frontend_test list-select "Lists expand on couplet lhs"
frontend_test args "Handling of arguments"

backend_test sh-simple uniq '(sh) "uniq . sort . grep . man"'
backend_test r-simple sqrt '(R) "sqrt . max . seq"'
backend_test sh-and-r grep '(sh,R) "grep . seq"'


echo
if [[ $n_fail == 0 ]]
then
    echo All tests pass
else
    warn "FAILURE"
    echo " - $n_fail out of $(( n_fail + n_pass )) tests failed"
fi
