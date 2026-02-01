all:
	rm -f obs.txt
	morloc make main.loc 
	./nexus foo0 "[1.0,2.0]" "[3.0,4.0]" >  obs.txt
	./nexus foo1 "[1.0,2.0]" "[3.0,4.0]" >> obs.txt
	./nexus foo2 "[1.0,2.0]" "[3.0,4.0]" >> obs.txt

clean:
	rm -f nexus* pool*
