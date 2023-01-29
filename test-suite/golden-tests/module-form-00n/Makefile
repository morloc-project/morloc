all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.pl foo 3 5 > obs.txt

clean:
	rm -f nexus* pool*
