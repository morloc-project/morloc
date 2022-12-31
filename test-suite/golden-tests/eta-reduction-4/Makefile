all:
	rm -f obs.txt
	morloc make main.loc
	./nexus.pl foo "[1.0,2.0,3.0]" > obs.txt

clean:
	rm -rf nexus* pool*
