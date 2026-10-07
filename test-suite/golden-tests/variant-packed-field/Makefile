all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus mk 3 >> obs.txt 2>> obs.err
	./nexus size '{"M":[[["a",1],["c",5],["d",6]],10]}' >> obs.txt 2>> obs.err
	./nexus size '"N"' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus __pycache__ *.err
	rm -rf *-build *build.tmp*
