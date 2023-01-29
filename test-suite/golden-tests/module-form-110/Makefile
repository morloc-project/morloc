all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.pl sum [1,2,3] > obs.txt

clean:
	rm -f nexus* pool*
