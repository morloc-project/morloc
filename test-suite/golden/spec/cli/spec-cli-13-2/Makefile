all:
	rm -f *.err obs.txt
	: > obs.txt
	if morloc make -o accept accept.loc 2> build.err; then echo OK >> obs.txt; else echo UNEXPECTED >> obs.txt; fi
	if morloc make -o tuple tuple.loc 2>> build.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if morloc make -o mapped mapped.loc 2>> build.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if morloc make -o nested nested.loc 2>> build.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if morloc make -o record record.loc 2>> build.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if morloc make -o inline inline.loc 2>> build.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if morloc make -o reserved reserved.loc 2>> build.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if morloc make -o option option.loc 2>> build.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if morloc make -o unrolled unrolled.loc 2>> build.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if morloc make -o check check.loc 2>> build.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if morloc make -o list list.loc 2>> build.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if morloc make -o many many.loc 2>> build.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if morloc make -o parse parse.loc 2>> build.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi

clean:
	rm -rf accept tuple mapped nested record inline reserved option unrolled check list many parse pools __pycache__
	rm -f *.err
	rm -rf *-build *build.tmp*
