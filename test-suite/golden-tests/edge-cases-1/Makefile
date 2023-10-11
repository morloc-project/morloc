all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus.py foo >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
