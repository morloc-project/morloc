all:
	rm -f obs.txt
	morloc make foo.loc 
	./nexus foo '[[["a","b"],[1,5]],[["c","d"],[2,6]]]' | sed 's/ //g' > obs.txt

clean:
	rm -f nexus* pool*
