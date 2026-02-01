all:
	rm -f obs.txt
	morloc make foo.loc 
	./nexus foo '[["a","b"],[[["3","4"],[2.48,1.2]],[["1","2"],[1.2,2.48]]]]' | sed 's/ //g' > obs.txt

clean:
	rm -f nexus* pool*
