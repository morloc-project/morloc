all:
	rm -f obs.txt
	morloc make foo.loc
	./nexus.pl foo '[["a","b"],[[1,2,3],[4,5,6]]]' | sed 's/ //g' > obs.txt

clean:
	rm -f nexus* pool*
