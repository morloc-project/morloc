all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus.pl siblings '"Eve"' > obs.txt

clean:
	rm -rf nexus* pool*
