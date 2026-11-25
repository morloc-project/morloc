export MORLOC_WRITE_BUFFER_BYTES = 4096

all:
	rm -f *.err obs.txt out.dat pieces.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus run out.dat >> obs.txt 2>> obs.err
	./nexus tally out.dat >> obs.txt 2>> obs.err
	./nexus runPieces pieces.dat >> obs.txt 2>> obs.err
	./nexus tallyPieces pieces.dat >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err out.dat pieces.dat
	rm -rf *-build *build.tmp*
