all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus runTuple 3 [10,20,30,40,50] 0 >> obs.txt 2>> obs.err
	./nexus runTuple 5 [1,2,3,4,5] 0 >> obs.txt 2>> obs.err
	./nexus runTuple 0 [7,8,9] 100 >> obs.txt 2>> obs.err
	./nexus runProj 3 [10,20,30,40,50] 0 >> obs.txt 2>> obs.err
	./nexus runProj 5 [1,2,3,4,5] 0 >> obs.txt 2>> obs.err
	./nexus runTriple 3 [10,20,30,40] 0 >> obs.txt 2>> obs.err
	./nexus runTriple 2 [5,6,7] 0 >> obs.txt 2>> obs.err
	./nexus runIO 4 [1,2,3,4,5] 0 >> obs.txt 2>> obs.err
	./nexus runIO 3 [10,20,30] 0 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
