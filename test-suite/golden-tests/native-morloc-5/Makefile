all:
	rm -f *.err obs.txt
	morloc make -o nexus high.loc 2> build.err
	./nexus foo [99,2] > obs.txt 2> obs.err

clean:
	rm -rf nexus pools
