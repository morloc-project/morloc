all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- defaults ---" >> obs.txt
	./nexus show >> obs.txt 2>> obs.err
	echo "--- several-after-one-flag ---" >> obs.txt
	./nexus show -l r py >> obs.txt 2>> obs.err
	echo "--- repeated-flag ---" >> obs.txt
	./nexus show -l r -l py >> obs.txt 2>> obs.err
	echo "--- string-tokens-stay-strings ---" >> obs.txt
	./nexus show -l 1 true '"q"' >> obs.txt 2>> obs.err
	echo "--- typed-elements-then-other-flags ---" >> obs.txt
	./nexus show --size 1 2 3 -n y -l cpp >> obs.txt 2>> obs.err
	echo "--- bad-element-refused ---" >> obs.txt
	if ./nexus show --size 1 two >> obs.err 2>&1; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
	rm -rf *-build *build.tmp*
