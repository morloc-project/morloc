all:
	rm -f obs.txt
	morloc make foo.loc
	./nexus.pl foo '{"name":"alice","info":[["a","b"],[1,2]]}' | sed 's/ //g' > obs.txt

clean:
	rm -rf nexus* *out
