all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err || true
	if grep -q "has a dimension no type here fixes" build.err; then echo "OK: refused" >> obs.txt; else echo "UNEXPECTED: not refused" >> obs.txt; fi
	morloc make -o nexus packable.loc 2> packable.err || true
	if grep -q "has a dimension no type here fixes" packable.err; then echo "OK: packable refused" >> obs.txt; else echo "UNEXPECTED: packable not refused" >> obs.txt; fi

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
