all:
	rm -f obs.txt
	morloc make foo.loc 2> build.err
	./nexus rms [1,2,3] > obs.txt 2> obs.err

clean:
	rm -rf nexus* pool* __pycache__
