all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus test >> obs.txt

clean:
	rm -f nexus* pool*
