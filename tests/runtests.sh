#!/usr/bin/env bash
set -u

usage (){
cat << EOF
Test suite for the LOC compiler
  -h      print this help message
  -q      quiet, print no output
  -x      stop on first failure
  -F      skip frontend tests
  -B      skip backend tests
  -T      skip type tests
  -K      skip tests of known problems
  -l LANG do test for language L
EOF
    exit 0
}

loc_flags=
backend_dir=

loud=true
instant_death=false
test_frontend=true
test_backend=true
test_types=true
test_known_problems=true 
lang="all"
while getopts "hqxFBKETl:" opt; do
    case $opt in
        h)
            usage ;;
        q)
            loud=false ;;
        x)
            instant_death=true ;;
        F)
            test_frontend=false ;;
        B)
            test_backend=false ;;
        T)
            test_types=false ;;
        K)
            test_known_problems=false ;;
        l)
            lang=$OPTARG ;;
        ?)
            warn "Illegal arguments"
            exit 1
    esac 
done

n_fail=0
n_pass=0

say(){
    $loud && echo "$@"
}

say_n(){
    $loud && echo -n "$@"
}

announce(){
    if $loud
    then
        [[ -t 1 ]] && o="\e[1;33m$1\e[0m" || o=$1
        echo -e $o
    fi
}

warn(){
    if $loud
    then
        [[ -t 1 ]] && o="\e[1;31m$1\e[0m" || o=$1
        echo -en $o
    fi
}

frontend_test(){
    dir=$1
    msg=$2
    cd frontend-tests/$dir

    say_n "$msg"

    loc -l $loc_flags x.loc &> /dev/null
    if [[ $? == 0 ]]
    then
        diff <(loc -l $loc_flags x.loc) x.lil > /dev/null
        if [[ $? == 0 ]]
        then
            say OK
            n_pass=$(( n_pass + 1 ))
        else
            warn FAIL
            say " - LIL diff"
            n_fail=$(( n_fail + 1 ))
        fi
    else
        warn FAIL
        say " - runtime"
        n_fail=$(( n_fail + 1 ))
        $instant_death && exit 1
    fi
    cd - &> /dev/null
}

# Success is determined by exit status of the x function
backend_x_test(){
    dir=$1
    msg=$2
    cd $backend_dir/$dir

    say_n "$msg"
    ./x &> /dev/null
    if [[ $? == 0 ]]
    then
        say OK
        n_pass=$(( n_pass + 1 ))
    else
        warn FAIL
        say " - $?"
        n_fail=$(( n_fail + 1 ))
        $instant_death && exit 1
    fi

    cd - &> /dev/null
}

backend_test(){
    dir="$1"
    cmd="$2"
    msg="$3"

    cd $backend_dir/$dir
    
    say_n "$msg"

    loc $loc_flags -kx tst x.loc &> /dev/null
    if [[ $? == 0 ]]
    then
        obs=/tmp/obs_$RANDOM
        exp=/tmp/exp_$RANDOM
        ./manifold-nexus.py "$cmd" &> $obs
        ./x > $exp 2> /dev/null

        diff $obs $exp &> /dev/null
        if [[ $? == 0 ]]
        then
            say OK
            n_pass=$(( n_pass + 1 ))
        else
            warn FAIL
            say " - diff"
            n_fail=$(( n_fail + 1 ))
        fi
        rm $obs $exp
    else
        warn FAIL
        say " - runtime"
        n_fail=$(( n_fail + 1 ))
        $instant_death && exit 1
    fi
    
    rm -rf tst manifold-nexus.py
    cd - &> /dev/null
}

if $test_frontend
then
announce "Frontend tests"
frontend_test types              'types/             -- types and lists of types ....................... '
frontend_test elements           'elements/          -- basic manifold elements exist .................. '
frontend_test operator-add       'operator-add/      -- test :=, and :+ ................................ '
frontend_test operator-sub       'operator-sub/      -- test :=, :+, and :- ............................ '
frontend_test stars              'stars/             -- test * on modifier lhs ......................... '
frontend_test include            'include/           -- can include files .............................. '
frontend_test list-select        'list-select/       -- lists expand on couplet lhs .................... '
frontend_test args               'args/              -- [+-]lhs, flags, lists .......................... '
frontend_test reset              'reset/             -- test RESET command ............................. '
frontend_test deref              'deref/             -- map . & (f . g . $1) . foo ..................... '
frontend_test grpref             'grpref/            -- f . *A ; A :: g ................................ '
frontend_test deref-mod          'deref-mod/         -- as above, but with effects on f ................ '
frontend_test selection          'selection/         -- @3 A/foo :+ a_foo .............................. '
frontend_test deep-path          'deep-path/         -- @3 A/B/foo :+ a_foo ............................ '
frontend_test nested-deref       'nested-deref/      -- f . ( &(g . h) ) ............................... '
frontend_test export-path        'export-path/       -- @export ; A/g as main .......................... '
frontend_test variable-names     'variable-names/    -- a2 . A4 . a-r a-14 ............................. '
fi

loc_flags=" -t "
backend_dir=type-tests
if $test_types
then
announce "Type tests"
backend_test multi         main 'multi/             -- types and lists of types ....................... '
backend_test sh-r-open     main 'sh-r-open/         -- send data from R to sh ......................... '
backend_test r-positionals main 'r-positionals/     -- replicate . `20` `sample` `letters` ............ '
fi


loc_flags=" "
backend_dir=backend-tests
if $test_backend
then
announce "Backend tests"

if [[ $lang == "all" || $lang == "R" ]] ; then
backend_x_test r-all                'r-all/             -- sqrt . max . seq ............................... '
backend_x_test r-memcache           'r-memcache/        -- null . xtable . data.frame . <runif> <runif> ... '
backend_x_test r-self-reference     'r-self-reference/  -- cat . <random> <random> ........................ '
backend_x_test r-logical            'r-logical/         -- and . is_a (any . is_b is_c (not . is_d)) ...... '
backend_x_test r-branch             'r-branch/          -- make if-elif-else analog with check ............ '
backend_x_test r-grpref-deref       'r-grpref-deref/    -- *X where X :: &( f . g . $1) ................... '
backend_test   r-map     main       'r-map/             -- simple test of lambda functions and map ........ '
backend_test   r-hooks   foo        'r-hooks/           -- run with all hooks ............................. '
backend_test   r-cached  sqrt       'r-cached/          -- sqrt . max . seq ............................... '
backend_test   r-check   sqrt       'r-check/           -- sqrt . max . seq ............................... '
backend_x_test r-refer              'r-refer/           -- max . <runif> .................................. '
backend_test   r-simple  sqrt       'r-simple/          -- sqrt . max . seq ............................... '
backend_x_test r-loop               'r-loop/            -- use open manifolds in map ...................... '
backend_x_test r-open-mod           'r-open-mod/        -- open manifold caching and modification ......... '
backend_test   r-single-quotes say  'r-single-quotes/   -- test nested single quotes ...................... '
fi

# if [[ $lang == "all" || $lang == "py" ]] ; then
# backend_test   py-hello-world  hi  'py-hello-world/    -- python hello world program ..................... '
# fi

if [[ $lang == "all" || $lang == "sh" ]] ; then
backend_test   sh-simple uniq       'sh-simple/         -- uniq . sort . grep . man ....................... '
backend_test   sh-all    uniq       'sh-all/            -- uniq . sort . grep . man ....................... '
backend_test   sh-map    main       'sh-map/            -- simple test of lambda functions and map ........ '
backend_test   sh-and-r  grep       'sh-and-r/          -- grep . seq ..................................... '
backend_test   sh-cached uniq       'sh-cached/         -- uniq . sort . grep . man ....................... '
backend_x_test sh-refer             'sh-refer/          -- head . <runif> ................................. '
backend_test   sh-simple uniq       'sh-simple/         -- uniq . sort . grep . man ....................... '
backend_test   sh-loop   map        'sh-loop/           -- map . &( cut . wc . grep . $1 ) ls . `*.sh` .... '
backend_x_test sh-arg-and-pos       'sh-arg-and-pos/    -- umm, I dont remember why I need this one ....... '
backend_x_test sh-hooks             'sh-hooks/          -- run with all hooks ............................. '
backend_x_test sh-open-mod          'sh-open-mod/       -- open manifold caching and modification ......... '
fi

fi

if $test_known_problems
then
if [[ $lang == "all" ]] ; then
announce "Known problems"
backend_test   sh-race         cat 'sh-race/           -- cat . <random> <random> ........................ '
backend_test   r-import        add 'r-import/          -- fanciful import statement ...................... '
fi
fi



say
if [[ $n_fail == 0 ]]
then
    say All tests pass
    exit 0
else
    warn "FAILURE"
    say " - $n_fail out of $(( n_fail + n_pass )) tests failed"
    exit 1
fi
