all:
	rm -f obs.txt
	morloc make foo.loc
	./nexus.pl foo '[[\"a\",\"b\"],[\{name=\"alice\",info=2\},\{name=\"bob\",info=3\}]]' > obs.txt

clean:
	rm -f nexus* pool*
