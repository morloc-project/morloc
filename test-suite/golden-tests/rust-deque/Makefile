all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus roundDeque  '[3,1,2]'          >> obs.txt 2>> obs.err
	./nexus dequeRecord '[4,5]'            >> obs.txt 2>> obs.err
	./nexus dequeConcat '[1,2]' '[3,4]'    >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
