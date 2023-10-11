all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus.py foo0 "[1.0,2.0]" "[3.0,4.0]" >  obs.txt
	./nexus.py foo1 "[1.0,2.0]" "[3.0,4.0]" >> obs.txt
	./nexus.py foo2 "[1.0,2.0]" "[3.0,4.0]" >> obs.txt

clean:
	rm -f nexus* pool*
