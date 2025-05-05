all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus foo a.json > obs.txt

clean:
	rm -rf nexus* pool* __pycache__
