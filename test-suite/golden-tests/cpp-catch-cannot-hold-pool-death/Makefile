all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	if ./nexus run 3 >> obs.txt 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	grep -c "'cpp'.*exited with status 70" obs.err >> obs.txt

clean:
	rm -f *.err nexus
	rm -rf *-build *build.tmp* __pycache__
