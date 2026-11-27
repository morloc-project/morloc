all:
	rm -f obs.txt *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus counts --n > obs.txt 2> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
