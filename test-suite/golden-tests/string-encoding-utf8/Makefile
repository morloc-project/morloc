all:
	rm -f obs.txt
	morloc make foo.loc 2> build.err
	./nexus foo a.json > obs.txt 2> obs.err

clean:
	rm -rf nexus* pool* __pycache__
