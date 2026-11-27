all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus nested >> obs.txt 2>> obs.err
	./nexus twoLevels >> obs.txt 2>> obs.err
	./nexus recordConstant >> obs.txt 2>> obs.err
	./nexus termHoldsConstant >> obs.txt 2>> obs.err
	./nexus bothArms true >> obs.txt 2>> obs.err
	./nexus bothArms false >> obs.txt 2>> obs.err
	./nexus whereBound >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
