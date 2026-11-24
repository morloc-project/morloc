STRIP = sed -e 's/mid=[0-9]*, //g'
all:
	rm -f obs.txt *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus nested 12 > obs.txt 2>> obs.err
	./nexus labels 6 2>&1 | $(STRIP) >> obs.txt
	./nexus forkFailUnderLock 0 >> obs.txt 2>> obs.err
	./nexus helperMask 0 >> obs.txt 2>> obs.err
	./nexus helperCount 0 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
