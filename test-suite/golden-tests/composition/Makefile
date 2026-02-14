all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo '["x","y"]' > obs.txt 2> obs.err
	./nexus bar 0 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus __pycache__
