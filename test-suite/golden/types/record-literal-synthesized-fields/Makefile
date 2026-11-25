all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus doTail 1   >  obs.txt 2>  obs.err
	./nexus letBody 2  >> obs.txt 2>> obs.err
	./nexus tryBody 3  >> obs.txt 2>> obs.err
	./nexus tupleCtl 6 >> obs.txt 2>> obs.err
	./nexus pureCtl 7  >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
