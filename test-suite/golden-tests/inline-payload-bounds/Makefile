all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus probePy -- -1 -1 >> obs.txt 2>> obs.err
	./nexus probePy -- 1048576 -1 >> obs.txt 2>> obs.err
	./nexus probePy -- 4503599627370496 -1 >> obs.txt 2>> obs.err
	./nexus probePy -- -1 1000000 >> obs.txt 2>> obs.err
	./nexus probeR -- -1 -1 >> obs.txt 2>> obs.err
	./nexus probeR -- 1048576 -1 >> obs.txt 2>> obs.err
	./nexus probeR -- 4503599627370496 -1 >> obs.txt 2>> obs.err
	./nexus probeR -- -1 1000000 >> obs.txt 2>> obs.err
	./nexus probeCpp -- -1 -1 >> obs.txt 2>> obs.err
	./nexus probeCpp -- 1048576 -1 >> obs.txt 2>> obs.err
	./nexus probeCpp -- 4503599627370496 -1 >> obs.txt 2>> obs.err
	./nexus probeCpp -- -1 1000000 >> obs.txt 2>> obs.err
	./nexus probeRust -- -1 -1 >> obs.txt 2>> obs.err
	./nexus probeRust -- 1048576 -1 >> obs.txt 2>> obs.err
	./nexus probeRust -- 4503599627370496 -1 >> obs.txt 2>> obs.err
	./nexus probeRust -- -1 1000000 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
