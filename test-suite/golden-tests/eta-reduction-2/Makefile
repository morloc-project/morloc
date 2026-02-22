all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus bar 1.0 2.0 3.0 4.0 > obs.txt 2> obs.err

clean:
	rm -rf nexus poolsgc
