all:
	rm -f obs.txt
	morloc make main.loc 2> build.err
	./nexus foo [1,2,3] > obs.txt 2> obs.err

clean:
	rm -rf nexus* pool* *pdf __pycache__
