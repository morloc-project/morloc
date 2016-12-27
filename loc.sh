usage (){
cat << EOF
Run the Rat compiler
  -l LANG select a backend (currently only R is supported)
  -L      no backend, print intermediate code
  -f PATH path to compiler frontend
  -b PATH path to compiler backend
  -h      print this help message and exit

rat code is read from STDIN and results written to STDOUT
EOF
    exit 0
}

lang=R
fpath=$HOME/bin/rat_frontend
bpath=$HOME/bin/rat_R_backend
while getopts "hl:Lf:b:" opt; do
    case $opt in
        h)
            usage ;;
        l)
            if [[ $OPTARG == 'R' || $OPTARG == 'r' ]]
            then
                lang=R
            else
                lang=NONE
            fi
            ;;
        L)
            lang=NONE
            ;;
        f)
            fpath=$OPTARG
            ;;
        b)
            bpath=$OPTARG
            ;;
    esac 
done

if [[ $lang == "R" ]]
then
    cat | $fpath | $bpath
elif [[ $land == "NONE" ]]
then
    cat | $fpath
fi
