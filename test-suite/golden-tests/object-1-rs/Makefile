all:
	rm -f obs.txt
	morloc make foo.loc
	./nexus.pl foo '{"name":"alice","info":34}' > obs.txt

clean:
	rm -rf nexus* *out
