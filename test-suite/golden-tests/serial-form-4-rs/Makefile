all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.py foo '[["b",55],["a",42]]' | sed 's/ //g' > obs.txt

clean:
	rm -rf nexus* *out
