all:
	rm -f obs.txt
	morloc make foo.loc 2> build.err
	bash run.sh > obs.txt 2> obs.err

clean:
	rm -rf nexus* pool* __pycache__ a.json b.json
