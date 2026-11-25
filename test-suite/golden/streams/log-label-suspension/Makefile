all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus direct 1 > direct.out 2> direct.err; cat direct.out >> obs.txt; python3 check.py 1 < direct.err >> obs.txt
	./nexus held 1 > held.out 2> held.err; cat held.out >> obs.txt; python3 check.py 2 < held.err >> obs.txt
	./nexus heldCpp 1 > heldcpp.out 2> heldcpp.err; cat heldcpp.out >> obs.txt; python3 check.py 2 < heldcpp.err >> obs.txt

clean:
	rm -rf nexus __pycache__ *.err *.out
	rm -rf *-build *build.tmp*
