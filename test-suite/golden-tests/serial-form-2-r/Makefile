all:
	rm -f obs.txt
	morloc make foo.loc
	./nexus.pl foo '[["b","a"],[55,42]]' > obs.txt

clean:
	rm -f nexus* pool*
