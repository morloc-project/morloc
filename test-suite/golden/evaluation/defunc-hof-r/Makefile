all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus applyLambda 10 >> obs.txt 2>> obs.err
	./nexus applyRuntimeCap 7 >> obs.txt 2>> obs.err
	./nexus applyMultiVar 3 4 >> obs.txt 2>> obs.err
	./nexus applyUnder 5 >> obs.txt 2>> obs.err
	./nexus applyReuse 3 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
