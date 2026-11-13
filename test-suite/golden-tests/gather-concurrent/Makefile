W := MORLOC_WRITE_BUFFER_BYTES=4096

all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	$(W) ./nexus threadsCpp 8 250 --tally >> obs.txt 2>> obs.err
	$(W) ./nexus threadsRust 8 250 --tally >> obs.txt 2>> obs.err
	$(W) ./nexus forksCpp 3 0 --sorted >> obs.txt 2>> obs.err
	$(W) ./nexus forksCpp 3 5000 --count >> obs.txt 2>> obs.err
	$(W) ./nexus forksPy 3 0 --sorted >> obs.txt 2>> obs.err
	$(W) ./nexus forksPy 3 5000 --count >> obs.txt 2>> obs.err
	$(W) ./nexus forksR 3 0 --sorted >> obs.txt 2>> obs.err
	$(W) ./nexus forksR 3 5000 --count >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
