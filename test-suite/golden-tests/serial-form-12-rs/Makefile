all:
	rm -f obs.txt
	morloc make foo.loc
	./nexus.pl foo '{"name":"alice","info":{"name":"bob","info":42}}' > obs.txt

clean:
	rm -f nexus* pool*
