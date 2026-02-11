all:
	rm -f obs.txt
	morloc make foo.loc 2> build.err
	./nexus foo '[["a","b"],[{"name":"alice","info":2},{"name":"bob","info":3}]]' | sed 's/ //g' > obs.txt 2> obs.err

clean:
	rm -f nexus* pool*
