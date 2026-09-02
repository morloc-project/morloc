all:
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus inputSum >> obs.txt 2> obs.err
	./nexus roundTripSum >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools build.err obs.err __pycache__
	rm -rf *-build *build.tmp*
