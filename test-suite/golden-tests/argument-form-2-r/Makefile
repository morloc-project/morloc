all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.pl foo 2 >> obs.txt

clean:
	rm -f nexus* pool*
