all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.pl foo '{"name":"alice","info":[["a","b"],[1,2]]}' | sed 's/ //g' > obs.txt

clean:
	rm -f nexus* pool*
