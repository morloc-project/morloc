all:
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus rEffect > obs.txt 2>> obs.err
	./nexus rPure >> obs.txt 2>> obs.err
	./nexus rAlias >> obs.txt 2>> obs.err
	./nexus rLambda >> obs.txt 2>> obs.err
	./nexus pyEffect >> obs.txt 2>> obs.err
	./nexus rViaHof >> obs.txt 2>> obs.err
	./nexus rViaList >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
	rm -rf *-build *build.tmp*
