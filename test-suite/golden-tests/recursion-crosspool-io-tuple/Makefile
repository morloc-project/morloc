all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus run 3 [10,20,30,40,50] 0 >> obs.txt 2>> obs.err
	./nexus run 5 [1,2,3,4,5] 0 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
