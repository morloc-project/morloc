all:
	rm -f *.err
	: > obs.txt
	rm -f /tmp/rust-handle-params-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus nextTwice      "/tmp/rust-handle-params-next.dat"   >> obs.txt 2>> obs.err
	./nexus drainSuspended "/tmp/rust-handle-params-drain.dat"  >> obs.txt 2>> obs.err
	./nexus lengthTwice    "/tmp/rust-handle-params-len.dat"    >> obs.txt 2>> obs.err
	./nexus streamTwice    "/tmp/rust-handle-params-stream.dat" >> obs.txt 2>> obs.err
	./nexus pairTwice      "/tmp/rust-handle-params-pair.dat"   >> obs.txt 2>> obs.err
	./nexus throughGeneric "/tmp/rust-handle-params-generic.dat" >> obs.txt 2>> obs.err
	rm -f /tmp/rust-handle-params-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/rust-handle-params-*.dat
	rm -rf *-build *build.tmp*
