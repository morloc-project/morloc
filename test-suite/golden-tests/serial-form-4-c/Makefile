all:
	rm -f *.err obs.txt
	morloc make -o nexus foo.loc 2> build.err
	./nexus foo '[["b",55],["a",42]]' | sed 's/ //g' > obs.txt 2> obs.err

clean:
	rm -f nexus
