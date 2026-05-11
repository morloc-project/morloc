all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- unitTop default (expect empty) ---"           >  obs.txt
	./nexus unitTop                                         >> obs.txt 2>  obs.err
	echo "--- unitTop --keep-null (expect null) ---"        >> obs.txt
	./nexus --keep-null unitTop                             >> obs.txt 2>> obs.err
	echo "--- optNone default (expect empty) ---"           >> obs.txt
	./nexus optNone                                         >> obs.txt 2>> obs.err
	echo "--- optNone --keep-null (expect null) ---"        >> obs.txt
	./nexus --keep-null optNone                             >> obs.txt 2>> obs.err
	echo "--- optSome (expect 7) ---"                       >> obs.txt
	./nexus optSome                                         >> obs.txt 2>> obs.err
	echo "--- pair (expect [5,null]) ---"                   >> obs.txt
	./nexus pair                                            >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
