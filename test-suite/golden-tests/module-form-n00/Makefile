all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus sub 3 5 > obs.txt

clean:
	rm -rf nexus* pool* __pycache__
