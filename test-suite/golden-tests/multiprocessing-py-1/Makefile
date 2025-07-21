all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus bar   [1,2,3] > obs.txt
	./nexus foo 5 [1,2,3] >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
