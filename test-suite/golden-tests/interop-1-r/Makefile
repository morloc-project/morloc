all:
	morloc make foo.loc
	./nexus.pl r2c 3 4 > obs.txt
	./nexus.pl c2r 3 4 >> obs.txt

clean:
	rm -f nexus* pool*
