all:
	rm -f *.err obs.txt data.bin
	morloc make -o nexus main.loc 2> build.err
	./nexus readBack data.bin > obs.txt 2>> obs.err
	./nexus readTwice data.bin >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err data.bin
	rm -rf *-build *build.tmp*
