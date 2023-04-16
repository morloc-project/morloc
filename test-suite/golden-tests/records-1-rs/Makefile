all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.py foo 5 > obs.txt

clean:
	rm -rf nexus* *out
