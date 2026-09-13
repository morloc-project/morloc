all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus rootReturn 1 >> obs.txt 2>> obs.err
	./nexus rootArg 1 10 >> obs.txt 2>> obs.err
	if morloc make -o nested-return nested-return.loc 2> nested-return.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if morloc make -o nested-arg nested-arg.loc 2> nested-arg.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi

clean:
	rm -rf nexus nested-return nested-arg __pycache__ *.err
	rm -rf *-build *build.tmp*
