all:
	rm -f obs.txt
	morloc make foo.loc
	./nexus.pl r2py 3 7 >> obs.txt
	./nexus.pl py2r 3 7 >> obs.txt

clean:
	rm -f nexus* pool*
