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
  -d      print diff statements
  -e      print error messages
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
difflog=/dev/null
errlog=/dev/null
lang="all"
while getopts "hqxdeFBKETl:" opt; do
    case $opt in
        h)
            usage ;;
        q)
            loud=false ;;
        x)
            instant_death=true ;;
        d)
            difflog=/dev/stdout ;;
        e)
            errlog=/dev/stdout ;;
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

    loc -l $loc_flags x.loc &> $errlog 
    if [[ $? == 0 ]]
    then
        diff <(loc -l $loc_flags x.loc) x.lil &> $difflog 
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
    ./x &> $errlog 
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

    loc $loc_flags -kx tst x.loc &> $errlog 
    if [[ $? == 0 ]]
    then
        obs=/tmp/obs_$RANDOM
        exp=/tmp/exp_$RANDOM
        ./manifold-nexus.py "$cmd" &> $obs
        ./x > $exp 2> /dev/null

        diff $obs $exp &> $difflog
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
frontend_test advice             'advice/            -- equivalence of (@before, @after) to (@4, @5) ... '
frontend_test complex-types      'complex-types/     -- test array, function and type inference ........ '
frontend_test lang-spec          'lang-spec/         -- language specific sections ..................... '
frontend_test multi              'multi/             -- test multi manifold type inference ............. '
frontend_test path-lang          'path-lang/         -- @path german ... @lang french .................. '
frontend_test positional-types   'positional-types/  -- test type inference of positional arguments .... '
frontend_test types              'types/             -- types and lists of types ....................... '
frontend_test elements           'elements/          -- basic manifold elements exist .................. '
frontend_test infer-star         'infer-star/        -- infer type of * generics ....................... '
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
frontend_test issue-2            'issue-2/           -- github issue #2 ................................ '
fi

loc_flags=" -t "
backend_dir=type-tests
if $test_types
then
announce "Type tests"
backend_test multi         main 'multi/             -- types and lists of types ....................... '
backend_x_test all              'all/               -- pass each atomic type through all language ..... '
backend_x_test all-vectors      'all-vectors/       -- pass each vector type through all language ..... '
backend_x_test all-tables       'all-tables/        -- pass each table type through all language ...... '
backend_x_test table-types      'table-types/       -- test coersion of Table type .................... '
backend_x_test deref            'deref/             -- send data from each language to sh ............. '
backend_x_test open-hooks       'open-hooks/        -- combinations of open manifolds with hooks ...... '
backend_test r-positionals main 'r-positionals/     -- replicate . `20` `sample` `letters` ............ '
backend_test tuples        main 'tuples/            -- pass tuple across all languages ................ '
fi


loc_flags=" "
backend_dir=backend-tests
if $test_backend
then
announce "Backend tests"

if [[ $lang == "all" || $lang == "R" ]] ; then
backend_test   r-cached        main 'r-cached/          -- sqrt . max . seq ............................... '
backend_x_test r-check              'r-check/           -- sqrt . max . seq ............................... '
backend_test   r-hooks         main 'r-hooks/           -- run with all hooks ............................. '
backend_test   r-map           main 'r-map/             -- simple test of lambda functions and map ........ '
backend_test   r-simple        main 'r-simple/          -- sqrt . max . seq ............................... '
backend_test   r-single-quotes main 'r-single-quotes/   -- test nested single quotes ...................... '
backend_x_test r-all                'r-all/             -- sqrt . max . seq ............................... '
backend_x_test r-branch             'r-branch/          -- make if-elif-else analog with check ............ '
backend_x_test r-grpref-deref       'r-grpref-deref/    -- *X where X :: &( f . g . $1) ................... '
backend_x_test r-logical            'r-logical/         -- and . is_a (any . is_b is_c (not . is_d)) ...... '
backend_x_test r-loop               'r-loop/            -- use open manifolds in map ...................... '
backend_x_test r-memcache           'r-memcache/        -- null . xtable . data.frame . <runif> <runif> ... '
backend_x_test r-open-mod           'r-open-mod/        -- open manifold caching and modification ......... '
backend_x_test r-refer              'r-refer/           -- max . <runif> .................................. '
backend_x_test r-self-reference     'r-self-reference/  -- cat . <random> <random> ........................ '
fi

if [[ $lang == "all" || $lang == "py" ]] ; then
backend_test   py-hooks       main  'py-hooks/          -- python hello world program ..................... '
backend_test   py-sh          main  'py-sh/             -- python [[Int]] to shell `sort` back to python .. '
backend_test   py-table       main  'py-table/          -- test printing of type `[[Int]]` ................ '
backend_test   py-positionals main  'py-positionals/    -- test type inference of positionals ............. '
backend_x_test py-logical           'py-logical/        -- and . is_a (any . is_b is_c (not . is_d)) ...... '
fi

if [[ $lang == "all" || $lang == "sh" ]] ; then
backend_test   r-sh-parallel main   'r-sh-parallel/     -- feed R composition into a bash map command ..... '
backend_test   sh-all        main   'sh-all/            -- uniq . sort . grep . man ....................... '
backend_test   sh-and-r      main   'sh-and-r/          -- grep . seq ..................................... '
backend_test   sh-cached     main   'sh-cached/         -- uniq . sort . grep . man ....................... '
backend_test   sh-loop       map    'sh-loop/           -- map . &( cut . wc . grep . $1 ) ls . `*.sh` .... '
backend_test   sh-map        main   'sh-map/            -- simple test of lambda functions and map ........ '
backend_test   sh-simple     main   'sh-simple/         -- uniq . sort . grep . man ....................... '
backend_x_test sh-arg-and-pos       'sh-arg-and-pos/    -- umm, I dont remember why I need this one ....... '
backend_x_test sh-hooks             'sh-hooks/          -- run with all hooks ............................. '
backend_x_test sh-logical           'sh-logical/        -- and . is_a (any . is_b is_c (not . is_d)) ...... '
backend_x_test sh-open-mod          'sh-open-mod/       -- open manifold caching and modification ......... '
backend_x_test sh-refer             'sh-refer/          -- head . <runif> ................................. '
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
