all:
	rm -f obs.txt
	morloc make foo.loc
	./nexus.pl foo [1,2,3]
	ls -s foo.pdf > obs.txt

clean:
	rm -f nexus* pool* *pdf
