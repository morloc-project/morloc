all:
	: > obs.txt
	rm -f *.err
	rm -f pkt-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus -f packet -z 0 -o pkt-small-z0.dat     smallList 2>> obs.err
	./nexus -f packet -z 3 -o pkt-small-z3.dat     smallList 2>> obs.err
	./nexus -f packet -z 0 -o pkt-big-shm-z0.dat   bigList   2>> obs.err
	./nexus -f packet -z 3 -o pkt-big-shm-z3.dat   bigList   2>> obs.err
	MORLOC_NO_SHM=1 ./nexus -f packet -z 0 -o pkt-big-noshm-z0.dat bigList 2>> obs.err
	MORLOC_NO_SHM=1 ./nexus -f packet -z 3 -o pkt-big-noshm-z3.dat bigList 2>> obs.err
	python3 verify.py >> obs.txt 2>> obs.err
	rm -f pkt-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f pkt-*.dat
