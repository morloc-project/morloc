all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus siblings '"Eve"' '[["Alice",42],["Bob",40]]' > obs.txt

clean:
	rm -rf nexus* pool*
