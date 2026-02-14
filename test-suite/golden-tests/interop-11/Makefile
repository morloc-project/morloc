all:
	rm -f *.err obs.txt *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus foo 14 > obs.txt 2> obs.err
	./nexus foo2 2 2>> obs.err  >> obs.txt
	./nexus foo3 3 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus __pycache__
