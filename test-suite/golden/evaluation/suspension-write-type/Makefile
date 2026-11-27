all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus saved out.bin >> obs.txt 2>> obs.err
	if morloc make -o unlifted-py unlifted-py.loc 2> unlifted-py.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if morloc make -o unlifted-cpp unlifted-cpp.loc 2> unlifted-cpp.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if morloc make -o unlifted-nexus unlifted-nexus.loc 2> unlifted-nexus.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi

clean:
	rm -rf nexus unlifted-py unlifted-cpp unlifted-nexus out.bin __pycache__ *.err
	rm -rf *-build *build.tmp*
