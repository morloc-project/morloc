all:
	rm -f obs.txt
	morloc make main.loc 2> build.err
	./nexus foo "true" > obs.txt 2> obs.err
	diff exp.txt obs.txt

clean:
	rm -rf nexus* pool* __pycache__
