all:
	rm -f *.err
	: > obs.txt
	rm -f /tmp/istream-rt-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus testNextOnce          "/tmp/istream-rt-next.dat"     >> obs.txt 2>> obs.err
	./nexus testNextEofTwice      "/tmp/istream-rt-eof.dat"      >> obs.txt 2>> obs.err
	./nexus testStreamFromIFile   "/tmp/istream-rt-stream.dat"   >> obs.txt 2>> obs.err
	./nexus testIntList           "/tmp/istream-rt-int.dat"      >> obs.txt 2>> obs.err
	printf '>sp|x\nACGT\n' > short.fa
	printf '>sp|A0A0A7EPL0|PIAL1_ARATH E4 SUMO-protein ligase\nMVIPATSRFGFRAEFNTKEF\n' > long.fa
	./nexus testOpenText          short.fa                       >> obs.txt 2>> obs.err
	./nexus testOpenText          long.fa                        >> obs.txt 2>> obs.err
	rm -f /tmp/istream-rt-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err short.fa long.fa
	rm -f /tmp/istream-rt-*.dat
	rm -rf *-build *build.tmp*
