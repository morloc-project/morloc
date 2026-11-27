all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	if ./nexus bad 1 > out.txt 2> obs.err; then echo UNEXPECTED > obs.txt; \
	elif grep -q 'Expected an instance of mlc_types._local.main.Pt for a record, but got dict' obs.err; then echo OK > obs.txt; \
	else echo UNEXPECTED > obs.txt; fi

clean:
	rm -rf nexus pools __pycache__ *err out.txt *-build *build.tmp*
