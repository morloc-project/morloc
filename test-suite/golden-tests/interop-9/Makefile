all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.py foo 1 > obs.txt

clean:
	rm -rf nexus* pool* __pycache__
