all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus setPyToCpp       >  obs.txt 2>  obs.err
	./nexus setCppToPy       >> obs.txt 2>> obs.err
	./nexus setPyToR         >> obs.txt 2>> obs.err
	./nexus setRToPy         >> obs.txt 2>> obs.err
	./nexus getPyToCpp       >> obs.txt 2>> obs.err
	./nexus getCppToR        >> obs.txt 2>> obs.err
	./nexus setNestedPyToCpp >> obs.txt 2>> obs.err
	./nexus setTwoCppToPy    >> obs.txt 2>> obs.err
	./nexus setThenGetAcross >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
	rm -f *.err
	rm -rf *-build *build.tmp*
