all:
	rm -f obs.txt
	morloc make foo.loc
	./nexus.pl foo '[[\"b\",[[\"c\",\"d\"],[1,5]]],[\"a\",[[\"e\"],[9]]]]' > obs.txt

clean:
	rm -f nexus* pool*
