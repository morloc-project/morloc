all:
	rm -f obs.txt
	morloc make foo.loc 
	./nexus foo '{"name":"alice","info":{"name":"bob","info":42}}' | sed 's/ //g' > obs.txt

clean:
	rm -f nexus* pool*
