all:
	rm -f obs.txt
	morloc make foo.loc
	./nexus.pl foo [1,1,2,3,5,8,13]
	ls *pdf > obs.txt

clean:
	rm -f nexus* pool* *pdf
