all:
	morloc make foo.loc
	./nexus.pl foo 12 > obs.txt
	./nexus.pl bar 42 >> obs.txt
	./nexus.pl baz 12 >> obs.txt

clean:
	rm -f nexus* pool* *gch
