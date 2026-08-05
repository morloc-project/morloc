all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus sumWithLet 0 100 >> obs.txt 2>> obs.err
	./nexus sumWithLet 0 100000 >> obs.txt 2>> obs.err
	./nexus collatz 27 >> obs.txt 2>> obs.err
	./nexus collatz 837799 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
