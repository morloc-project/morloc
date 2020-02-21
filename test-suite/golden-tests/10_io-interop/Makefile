all:
	rm -f obs.txt
	morloc make foo.loc
	./nexus.pl fibplot 5
	ls -s *.pdf > obs.txt

clean:
	rm -f nexus* pool* *pdf
