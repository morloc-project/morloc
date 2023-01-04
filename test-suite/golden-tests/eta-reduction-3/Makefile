all:
	rm -f obs.txt
	morloc make main.loc
	./nexus.pl foo0 2 > obs.txt
	./nexus.pl foo1 2 >> obs.txt
	./nexus.pl foo2 2 >> obs.txt

clean:
	rm -rf nexus* pool*
