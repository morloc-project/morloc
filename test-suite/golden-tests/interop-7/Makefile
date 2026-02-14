all:
	rm -f *.err obs.txt
	morloc make -o nexus foo.loc 2> build.err
	./nexus foo 13 > obs.txt 2> obs.err
	diff exp.txt obs.txt

clean:
	rm -rf nexus pools
