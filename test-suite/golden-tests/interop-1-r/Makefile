all:
	rm -f obs.txt
	morloc make foo.loc 
	./nexus r2c 3 4 > obs.txt
	./nexus c2r 3 4 >> obs.txt

clean:
	rm -f nexus* pool*
