all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus.pl foo 6 > obs.txt

clean:
	rm -rf nexus* pool*
