all:
	rm -f *.err obs.txt
	morloc make -o nexus foo.loc 2> build.err
	./nexus foo > obs.txt 2> obs.err

clean:
	rm -rf nexus __pycache__
