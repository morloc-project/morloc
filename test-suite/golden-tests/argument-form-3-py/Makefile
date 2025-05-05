all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus foo 2 >> obs.txt

clean:
	rm -f nexus* pool*
