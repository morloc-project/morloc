all:
	rm -f obs.txt
	morloc make foo.loc
	./nexus.pl foo '[[\"b\",55],[\"a\",42]]' > obs.txt

clean:
	rm -f nexus* pool*
