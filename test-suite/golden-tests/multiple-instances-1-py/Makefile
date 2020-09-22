all:
	rm -f obs.txt
	morloc make foo.loc
	./nexus.pl rms [1,2,3] > obs.txt

clean:
	rm -rf nexus* pool* __pycache__
