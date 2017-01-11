#!/usr/bin/env bash

set -u

usage (){
cat << EOF
DESC
    Process LOC file into ultimate executable
REQUIRED ARGUMENTS
  -l LOC the loc script
  -L print the output LIL (after removing source)
  -G print the m4 text before macro expansion
  -R print the derived macro rules
EOF
    exit 0
}

# print help with no arguments
[[ $# -eq 0 ]] && usage

print_lil=false
print_m4=false
print_rules=false
while getopts "hLGR" opt; do
    case $opt in
        h)
            usage ;;
        L)
            print_lil=true ;;
        G)
            print_m4=true ;; 
        R)
            print_rules=true ;;
    esac 
done

locsrc=${@:$OPTIND:1}

grammar=etc/grammar.m4

bash_prologue() {
    echo -n
}

bash_epilogue() {
cat << EOF
if manifold_exists \$1
then
    \$1 
else
    exit 1 
fi
EOF
}


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


# === synopsis =========================
# exe . cat . src man
# man . m4 . cat . grammar rules body
# rules body . parser . loc
# ======================================


tmp=`make_temp_dir`

lang="bash"

lib=lib/$lang-base.loc

loc=$tmp/loc # Full input loc script (including base libraries)
lil=$tmp/lil # Loc Intermediate Language (output of Loc compiler)
exe=$tmp/exe # The final executable for this language
src=$tmp/src # Source code for this language extracted from LIL
red=$tmp/red # reduced lil (after removing source code)

pro=$tmp/pro # executable header material (prologue)
bash_prologue > $pro
epi=$tmp/epi # executable tail material (epilogue)
bash_epilogue > $epi

# Production rules - includes rules from the grammar and runtime macros
rules=$tmp/rules

# M4 body that will be generated into the body of the ultimate source file
body=$tmp/body

# manifold functions after macro expansions
man=$tmp/man


# Parse LOC into LIL
cat $lib $locsrc > $loc
loc $loc > $lil


# - Extract LANG source from LIL
# - Write this source to a temporary `src` file
# - Write LIL without source to temporary `red` file
>$src >$red
awk -v src="$src" -v red="$red" -v lng="bash" '
    BEGIN{ state=0 }
    $0 ~ /^[^ ]/ { state = 0 }
    $1 == "SOURCE" && $2 ~ lng { state = 1; next; }
    state == 1 { print >> src }
    state == 0 && $0 ~ /^[^ ]/ { print > red }
' $lil

# replace original LIL with the sourceless LIL
mv $red $lil

# Build macros and rules
./parse.awk -v rules=$rules -v body=$body -v L='`' -v R="'" < $lil

# Merge all rules and macros, expand to manifold functions;
cat $grammar $rules $body | m4 > $man


# Combine source and manifold functions into final executable
cat $pro $src $man $epi | sed '/^ *$/d;s/ *//' > $exe


# Move executable to working folder and set permissions
cp $exe call-bash.sh
chmod 755 call-bash.sh

# Optional debugging output
if $print_lil
then
    cat $lil
fi

if $print_rules
then
    cat $rules
fi

if $print_m4
then
    cat $grammar
    echo
    cat $rules
    echo
    cat $body
fi

# cleanup
rm -rf $tmp
