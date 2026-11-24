all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	for c in throws calleeDies ; do echo "--- $$c ---" >> obs.txt ; if ulimit -c 0 && ./nexus $$c 3 >> obs.txt 2> run.err ; then echo UNEXPECTED >> obs.txt ; else echo OK >> obs.txt ; fi ; grep -c "exited with status 70" run.err >> obs.txt ; grep -c "signal" run.err >> obs.txt ; cat run.err >> obs.err ; done ; true

clean:
	rm -f *.err nexus
	rm -rf *-build *build.tmp* __pycache__
