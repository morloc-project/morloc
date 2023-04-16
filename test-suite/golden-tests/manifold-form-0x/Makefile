all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.py foo 12 > obs.txt

clean:
	rm -f nexus* pool* *pdf
