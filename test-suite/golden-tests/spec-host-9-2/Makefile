all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus madeHome 3 >> obs.txt 2> made.err
	grep -c made made.err >> obs.txt
	./nexus madeDirect 3 >> obs.txt 2>> obs.err
	morloc make -o bad bad.loc 2> bad.err || true
	if grep -q "would be passed into cpp" bad.err; then echo "OK: a function made by Python is not sent to C++" >> obs.txt; else echo "UNEXPECTED: bad.loc was not refused" >> obs.txt; fi

clean:
	rm -rf nexus bad pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
