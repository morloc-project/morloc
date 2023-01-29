all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.pl py2c 3 4 > obs.txt
	./nexus.pl c2py 3 4 >> obs.txt

clean:
	rm -f nexus* pool*
