all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus foo0 2 > obs.txt
	./nexus foo1 2 >> obs.txt
	./nexus foo2 2 >> obs.txt

clean:
	rm -f nexus* pool*
