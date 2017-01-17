#!/usr/bin/env bash

set -u

M4_FLAGS="--traditional --prefix-builtins --warn-macro-sequence --hashsize 7919"

make_temp_dir() {
    i=0
    while [[ -d "/tmp/loc_$i" ]]
    do
        i=$(( i + 1 ))
    done
    tmp=/tmp/loc_$i
    mkdir $tmp
    echo $tmp
}

exit_and_clean() {
    rm -rf $tmp
    exit $1
}

find_all_languages() {
    awk -F"\t" '$1 == "EMIT" && $3 != "*" { print $3 }' $1 | sort -u
}

usage (){
cat << EOF
DESC
    Process LOC file into ultimate executable
USAGE
   ./build.sh [options] whatever.loc
OPTIONS
  -o     DIR build workflow in DIR
  -l     only compile LIL
  -L     print the output LIL (after removing source)
  -G     print the m4 text before macro expansion
  -R     print the derived macro rules
  -x MID build, call MID, and remove
  -n NEX manifold nexus file name
COMPILER OPTIONS
  -c     run the typechecker on manifolds
  -t     print tokens to a token file
  -d     recursively dump symbol table
EOF
    exit 0
}

home=~/.loc
loc_compiler=$home/bin/loc
parse=$home/bin/parse-grammar.awk


# print help with no arguments
[[ $# -eq 0 ]] && usage

tmp=`make_temp_dir`
outdir=`basename $tmp`
flags=
symdump=false
execute=false execute_id=
only_lil=false
print_lil=false
print_m4=false
print_rules=false
nexus_given=false
while getopts "hLGRlctdx:o:n:" opt; do
    case $opt in
        h)
            usage ;;
        o)
            outdir=$OPTARG ;;
        n)
            nexus_given=true
            nexus=$OPTARG ;;
        l)
            only_lil=true ;;
        L)
            print_lil=true ;;
        G)
            print_m4=true ;; 
        R)
            print_rules=true ;;
        c)
            flags="$flags -c" ;;
        t)
            flags="$flags -t" ;;
        d)
            flags="$flags -d"
            symdump=true ;;
        x)
            execute_id=$OPTARG
            execute=true ;;
    esac 
done

locsrc=${@:$OPTIND:1}

$only_lil && $loc_compiler $locsrc && exit_and_clean 0

$symdump && $loc_compiler $flags $locsrc && exit_and_clean 0

lil=$tmp/lil # Loc Intermediate Language (output of Loc compiler)
$loc_compiler $flags $locsrc > $lil

mkdir $outdir
mkdir $outdir/cache

$nexus_given || nexus=$outdir/manifold-nexus.sh

for lang in $(find_all_languages <($loc_compiler $locsrc))
do
    exe=$tmp/exe # The final executable for this language
    src=$tmp/src # Source code for this language extracted from LIL
    red=$tmp/red # reduced lil (after removing source code)

    # Production rules - includes rules from the grammar and runtime macros
    rules=$tmp/rules

    grammar=$home/etc/grammar/$lang.m4

    # M4 body that will be generated into the body of the ultimate source file
    body=$tmp/body

    # manifold functions after macro expansions
    man=$tmp/man

    loc=$tmp/loc

    >$exe >$src >$red >$rules >$body >$man

    # - Extract LANG source from LIL
    # - Write this source to a temporary `src` file
    # - Write LIL without source to temporary `red` file
    awk -v src="$src" -v red="$red" -v lng=$lang '
        BEGIN{ FS="\t" ; state=0 }
        $0 ~ /^[^ ]/ { state = 0 }
        $1 == "SOURCE" && $2 ~ lng { state = 1; next; }
        state == 1 { print > src }
        state == 0 && $0 ~ /^[^ ]/ { print > red }
    ' $lil

    # Build macros and rules
    $parse              \
        -v lang=$lang   \
        -v rules=$rules \
        -v body=$body   \
        -v dir=$outdir  \
        -v src=$src     \
        -v L='`' -v R="'" < $red

    # Merge all rules and macros, expand to manifold functions
    cat $grammar $rules $body |
        m4 $M4_FLAGS |
        sed '/^ *$/d;s/ *//' > $exe

    # Move executable to working folder and set permissions
    cp $exe $outdir/call.$lang
    chmod 755 $outdir/call.$lang

    if $print_rules
    then
        echo "### $lang rules:"
        cat $rules
    fi

    if $print_m4
    then
        echo "### $lang m4:"
        cat $grammar
        echo
        cat $rules
        echo
        cat $body
    fi

done

nexus_grammar=$HOME/.loc/etc/nexus.m4
nexus_rules=$tmp/nexus_rules
nexus_body=$HOME/.loc/etc/nexus.sh
build_nexus=$HOME/.loc/bin/build-nexus.awk
$build_nexus                  \
    -v dir=$PWD/$outdir       \
    -v name=`basename $nexus` \
    -v lang=$lang             \
    $lil > $nexus_rules

cat $nexus_grammar $nexus_rules $nexus_body |
    m4 $M4_FLAGS |
    sed -n '/./,$p' > $nexus
chmod 755 $nexus

if $execute
then
    $nexus $execute_id
    rm -rf $outdir
fi

# Optional debugging output
if $print_lil
then
    cat $red
fi

exit_and_clean 0
