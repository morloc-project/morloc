all:
	rm -f *.err obs.txt
	morloc make -o nexus-py main-py.loc 2>> build.err
	morloc make -o nexus-cpp main-cpp.loc 2>> build.err
	morloc make -o nexus-r main-r.loc 2>> build.err
	morloc make -o nexus-rust main-rust.loc 2>> build.err
	echo "--- nexus-py ---" >> obs.txt
	./nexus-py index 3 >> obs.txt 2>> obs.err
	./nexus-py viaHelper 3 >> obs.txt 2>> obs.err
	./nexus-py twoArg 3 >> obs.txt 2>> obs.err
	./nexus-py curried 3 >> obs.txt 2>> obs.err
	./nexus-py partialTwoArg 3 >> obs.txt 2>> obs.err
	./nexus-py mapIndex 3 >> obs.txt 2>> obs.err
	./nexus-py sliceThenIndex 3 >> obs.txt 2>> obs.err
	./nexus-py tupleGet 3 >> obs.txt 2>> obs.err
	./nexus-py mapTupleGet 3 >> obs.txt 2>> obs.err
	echo "--- nexus-cpp ---" >> obs.txt
	./nexus-cpp index 3 >> obs.txt 2>> obs.err
	./nexus-cpp viaHelper 3 >> obs.txt 2>> obs.err
	./nexus-cpp twoArg 3 >> obs.txt 2>> obs.err
	./nexus-cpp curried 3 >> obs.txt 2>> obs.err
	./nexus-cpp partialTwoArg 3 >> obs.txt 2>> obs.err
	./nexus-cpp mapIndex 3 >> obs.txt 2>> obs.err
	./nexus-cpp sliceThenIndex 3 >> obs.txt 2>> obs.err
	./nexus-cpp tupleGet 3 >> obs.txt 2>> obs.err
	./nexus-cpp mapTupleGet 3 >> obs.txt 2>> obs.err
	echo "--- nexus-r ---" >> obs.txt
	./nexus-r index 3 >> obs.txt 2>> obs.err
	./nexus-r viaHelper 3 >> obs.txt 2>> obs.err
	./nexus-r twoArg 3 >> obs.txt 2>> obs.err
	./nexus-r curried 3 >> obs.txt 2>> obs.err
	./nexus-r partialTwoArg 3 >> obs.txt 2>> obs.err
	./nexus-r mapIndex 3 >> obs.txt 2>> obs.err
	./nexus-r sliceThenIndex 3 >> obs.txt 2>> obs.err
	./nexus-r tupleGet 3 >> obs.txt 2>> obs.err
	./nexus-r mapTupleGet 3 >> obs.txt 2>> obs.err
	echo "--- nexus-rust ---" >> obs.txt
	./nexus-rust index 3 >> obs.txt 2>> obs.err
	./nexus-rust viaHelper 3 >> obs.txt 2>> obs.err
	./nexus-rust twoArg 3 >> obs.txt 2>> obs.err
	./nexus-rust curried 3 >> obs.txt 2>> obs.err
	./nexus-rust partialTwoArg 3 >> obs.txt 2>> obs.err
	./nexus-rust mapIndex 3 >> obs.txt 2>> obs.err
	./nexus-rust sliceThenIndex 3 >> obs.txt 2>> obs.err
	./nexus-rust tupleGet 3 >> obs.txt 2>> obs.err
	./nexus-rust mapTupleGet 3 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus-py nexus-cpp nexus-r nexus-rust pools __pycache__
	rm -f *.err
	rm -rf *-build *build.tmp*
