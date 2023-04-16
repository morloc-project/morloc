all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.py py2c 3 4 > obs.txt
	./nexus.py c2py 3 4 >> obs.txt

clean:
	rm -f nexus* pool*
