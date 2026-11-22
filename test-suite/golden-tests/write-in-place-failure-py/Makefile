export MORLOC_WRITE_BUFFER_BYTES = 4096

all:
	rm -f *.err obs.txt out.dat reals.dat i8.dat bytes.dat arrays.dat arrays2.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus run out.dat >> obs.txt 2>> obs.err
	./nexus tally out.dat >> obs.txt 2>> obs.err
	./nexus reals reals.dat >> obs.txt 2>> obs.err
	./nexus readReals reals.dat >> obs.txt 2>> obs.err
	./nexus i8s i8.dat >> obs.txt 2>> obs.err
	./nexus countI8s i8.dat >> obs.txt 2>> obs.err
	./nexus bytesRun bytes.dat >> obs.txt 2>> obs.err
	./nexus tally bytes.dat >> obs.txt 2>> obs.err
	./nexus arrays arrays.dat >> obs.txt 2>> obs.err
	./nexus readArrays arrays.dat >> obs.txt 2>> obs.err
	./nexus arraysWhy arrays2.dat >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err out.dat reals.dat i8.dat bytes.dat arrays.dat arrays2.dat
	rm -rf *-build *build.tmp*
