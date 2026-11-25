all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err || true
	if grep -q "have conflicting definitions of the term" build.err; then echo "OK: refused" >> obs.txt; else echo "UNEXPECTED: not refused" >> obs.txt; fi

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
