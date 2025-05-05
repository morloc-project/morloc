all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus py2c 3 4 > obs.txt
	./nexus c2py 3 4 >> obs.txt

clean:
	rm -f nexus* pool*
