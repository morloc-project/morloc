all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.py person '"Bob"' >> obs.txt

clean:
	rm -f nexus* pool*
