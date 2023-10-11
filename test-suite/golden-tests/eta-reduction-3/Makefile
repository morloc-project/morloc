all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus.py foo0 2 > obs.txt
	./nexus.py foo1 2 >> obs.txt
	./nexus.py foo2 2 >> obs.txt

clean:
	rm -f nexus* pool*
