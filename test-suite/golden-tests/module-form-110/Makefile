all:
	rm -f obs.txt
	morloc make main.loc 2> build.err
	./nexus foo 5 7 > obs.txt 2> obs.err

clean:
	rm -rf nexus* pool* __pycache__
