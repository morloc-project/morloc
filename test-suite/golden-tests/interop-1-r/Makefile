all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.py r2c 3 4 > obs.txt
	./nexus.py c2r 3 4 >> obs.txt

clean:
	rm -f nexus* pool*
