all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.py foo [1,3,6] [7,7,7] > obs.txt

clean:
	rm -f nexus* pool*
