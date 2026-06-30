all:
	rm -f *.err
	: > obs.txt
	rm -f /tmp/ostream-cpp-pool-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus testPoolWriteRoundtrip "/tmp/ostream-cpp-pool-rt.dat"    >> obs.txt 2>> obs.err
	./nexus testPoolWriteMulti     "/tmp/ostream-cpp-pool-multi.dat" >> obs.txt 2>> obs.err
	rm -f /tmp/ostream-cpp-pool-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/ostream-cpp-pool-*.dat
