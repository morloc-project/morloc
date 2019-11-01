all:
	rm -f obs.txt
	morloc make simple.loc
	./nexus.pl cor [1,2,3] [1,3,3] > obs.txt
	./nexus.pl foo 16 >> obs.txt

clean:
	rm -f nexus* pool*
