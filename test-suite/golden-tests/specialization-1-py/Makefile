all:
	morloc make foo.loc
	./nexus.py foo [1,2,3] > obs.txt
	./nexus.py square [1,2,3] >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
