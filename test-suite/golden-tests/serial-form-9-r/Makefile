all:
	rm -f obs.txt
	morloc make foo.loc 
	./nexus foo '[["a","b"],[{"name":"alice","info":2},{"name":"bob","info":3}]]' | sed 's/ //g' > obs.txt

clean:
	rm -f nexus* pool*
