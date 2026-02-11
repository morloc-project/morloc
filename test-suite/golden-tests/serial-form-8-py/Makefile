all:
	rm -f obs.txt
	morloc make foo.loc 2> build.err
	./nexus foo '[["a","b"],[[[3,4],[2.48,1.2]],[[1,2],[1.2,2.48]]]]' | sed 's/ //g' > obs.txt 2> obs.err

clean:
	rm -f nexus* pool*
