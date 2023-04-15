all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.py foo [1,2,3] [2,3,4] >> obs.txt

clean:
	rm -rf nexus* *out
