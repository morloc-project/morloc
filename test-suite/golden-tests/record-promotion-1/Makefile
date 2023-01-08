all:
	rm -f obs.txt
	morloc make main.loc
	./nexus.pl siblings '"Eve"' > obs.txt

clean:
	rm -rf nexus* pool*
