all:
	rm -f obs.txt
	morloc make foo.loc
	./nexus.pl foo '[[\"b\",\"a\"],[[[1,2],[1.2,2.48]],[[3,4],[2.48,1.2]]]]' > obs.txt

clean:
	rm -f nexus* pool*
