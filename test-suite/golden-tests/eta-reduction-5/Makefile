all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus.pl foo0 "[1.0,2.0]" "[3.0,4.0]" >  obs.txt
	./nexus.pl foo1 "[1.0,2.0]" "[3.0,4.0]" >> obs.txt
	./nexus.pl foo2 "[1.0,2.0]" "[3.0,4.0]" >> obs.txt

clean:
	rm -rf nexus* pool*
