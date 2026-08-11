all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus collatzX 6 0 >> obs.txt 2>> obs.err
	./nexus collatzX 27 0 >> obs.txt 2>> obs.err
	./nexus fibCross 0 1 10 >> obs.txt 2>> obs.err
	./nexus fibCross 0 1 20 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
