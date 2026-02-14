all:
	rm -f *.err obs.txt
	morloc make -o nexus foo.loc 2> build.err
	./nexus dobytes '"asdf"' > obs.txt 2> obs.err
	./nexus doarray '"asdf"' 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus __pycache__
