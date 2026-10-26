all:
	rm -f obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus hoisted 3 > obs.txt 2> obs.err
	./nexus recursive 4 >> obs.txt 2>> obs.err
	./nexus twoHosts 4 >> obs.txt 2>> obs.err
	./nexus mutual 4 >> obs.txt 2>> obs.err
	./nexus listed 3 >> obs.txt 2>> obs.err
	if morloc make -o bad bad.loc 2> bad.err; then echo UNEXPECTED >> obs.txt; elif grep -q 'cannot leave the pool that made it' bad.err; then echo OK >> obs.txt; else echo UNEXPECTED >> obs.txt; fi
	if morloc make -o bad-closure bad-closure.loc 2> bad-closure.err; then echo UNEXPECTED >> obs.txt; elif grep -q 'cannot leave the pool that made it' bad-closure.err; then echo OK >> obs.txt; else echo UNEXPECTED >> obs.txt; fi

clean:
	rm -rf nexus bad bad-closure pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
