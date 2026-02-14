all:
	rm -f *.err obs.txt
	morloc make -o nexus foo.loc 2> build.err
	./nexus foo 42 > obs.txt 2> obs.err
	./nexus bar 42 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus __pycache__
