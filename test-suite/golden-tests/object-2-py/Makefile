all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.pl foo '"alice"' 34 > obs.txt

clean:
	rm -f nexus* pool*
