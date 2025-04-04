all:
	morloc make foo.loc
	./nexus foo [1,2,3] > obs.txt
	./nexus square [1,2,3] >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
