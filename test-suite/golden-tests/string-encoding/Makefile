all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	>obs.txt
	./nexus.py foop a.json >> obs.txt
	./nexus.py foor a.json >> obs.txt
	./nexus.py fooc a.json >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
