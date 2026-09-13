all:
	rm -f *.err
	: > obs.txt
	rm -rf cache disk
	mkdir -p cache disk
	morloc make -o save save.loc 2> save.err
	./save put disk/x 41 > /dev/null 2>> obs.err
	morloc make -o nexus main.loc 2> build.err
	./nexus robust x >> obs.txt 2>> obs.err
	./save put cache/x 7 > /dev/null 2>> obs.err
	./nexus robust x >> obs.txt 2>> obs.err
	if morloc make -o unlifted-pool unlifted-pool.loc 2> unlifted-pool.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if morloc make -o unlifted-nexus unlifted-nexus.loc 2> unlifted-nexus.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi

clean:
	rm -rf nexus save unlifted-pool unlifted-nexus cache disk __pycache__ *.err
	rm -rf *-build *build.tmp*
