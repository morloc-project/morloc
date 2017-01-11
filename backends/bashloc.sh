#!/usr/bin/env bash

set -u

bash_prelude() {
    echo 'run_mid=$0'
    echo
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

locsrc="$1" # input Loc file
grammar="$2" # an M4 grammar file

lib=lib-$lang/base.loc

loc=$tmp/loc # Full input loc script (including base libraries)
lil=$tmp/lil # Loc Intermediate Language (output of Loc compiler)
exe=$tmp/exe # The final executable for this language
src=$tmp/src # Source code for this language extracted from LIL
red=$tmp/red # reduced lil (after removing source code)

# Production rules - includes rules from the grammar and runtime macros
rules=$tmp/rules

# M4 body that will be generated into the body of the ultimate source file
body=$tmp/body

# manifold functions after macro expansions
man=$tmp/man


# Parse LOC into LIL
cat $lib $locsrc > $loc
loc $loc > $lil


# Write header material to the source file
bash_prelude > $src


# - Extract LANG source from LIL
# - Write this source to a temporary `src` file
# - Write LIL without source to temporary `red` file
>$red
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

cat $lil

# Merge all rules and macros, expand to manifold functions;
cat $grammar $rules $body | m4 > $man


# Combine source and manifold functions into final executable
cat $src $man | sed '/^ *$/d;s/ *//' > $exe


# Move executable to working folder and set permissions
cp $exe exe-bash.sh
chmod 755 exe-bash.sh


# cleanup
rm -rf $tmp
