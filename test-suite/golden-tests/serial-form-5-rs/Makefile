all:
	rm -f obs.txt
	morloc make foo.loc
	./nexus.pl foo '[[["a","b"],[1,5]],[["c","d"],[2,6]]]' | sed 's/ //g' > obs.txt

clean:
	rm -rf nexus* *out
