all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	>obs.txt
	./nexus foop a.json >> obs.txt
	./nexus foor a.json >> obs.txt
	./nexus fooc a.json >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
