all:
	rm -f obs.txt
	morloc make foo.loc 2> build.err
	./nexus bar   [1,2,3] > obs.txt 2> obs.err
	./nexus foo 5 [1,2,3] 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
