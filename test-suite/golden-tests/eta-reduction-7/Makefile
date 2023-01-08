all:
	rm -f obs.txt
	morloc make main.loc
	./nexus.pl bar 1 [2,3,4] > obs.txt
	./nexus.pl foo 1 [2,3,4] >> obs.txt

clean:
	rm -rf nexus* pool*
