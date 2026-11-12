export MORLOC_WRITE_BUFFER_BYTES = 4096

all:
	rm -f *.err obs.txt out.dat i8.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus run out.dat >> obs.txt 2>> obs.err
	./nexus tally out.dat >> obs.txt 2>> obs.err
	./nexus i8s i8.dat >> obs.txt 2>> obs.err
	./nexus countI8s i8.dat >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err out.dat i8.dat
	rm -rf *-build *build.tmp*
