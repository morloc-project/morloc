all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus baseEffect 5 >> obs.txt 2>> obs.err
	./nexus perIter 3 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
