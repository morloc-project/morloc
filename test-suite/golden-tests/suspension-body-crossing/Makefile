all:
	rm -f obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus inR 1 > obs.txt 2> obs.err
	./nexus inCpp 1 >> obs.txt 2>> obs.err
	./nexus inRust 1 >> obs.txt 2>> obs.err
	./nexus inPy 1 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
