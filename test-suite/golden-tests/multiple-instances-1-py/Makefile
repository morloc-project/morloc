all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus rms [1,2,3] > obs.txt

clean:
	rm -rf nexus* pool* __pycache__
