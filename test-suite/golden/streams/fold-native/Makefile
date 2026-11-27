all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	for c in emptyCpp emptyPy emptyR emptyRust emptyForkCpp emptyForkPy emptyFilePy emptyFileR threadsCpp threadsPy threadsR threadsRust inplacePy lateCpp ; do echo "--- $$c ---" >> obs.txt ; ./nexus $$c --n >> obs.txt 2>> obs.err ; done ; true

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
