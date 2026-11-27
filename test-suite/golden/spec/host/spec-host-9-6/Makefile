all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus useReal 3 2.5 >> obs.txt 2>> obs.err
	morloc make -o bad bad.loc 2> bad.err || true
	if grep -q "cannot leave the pool that made it" bad.err; then echo "OK: bad refused" >> obs.txt; else echo "UNEXPECTED: bad was not refused" >> obs.txt; fi

clean:
	rm -rf nexus bad pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
