all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.pl foo '[["b",55],["a",42]]' | sed 's/ //g' > obs.txt

clean:
	rm -f nexus* pool*
