all:
	rm -f obs.txt
	morloc make main.loc 2> build.err
	./nexus foo0 2 > obs.txt 2> obs.err
	./nexus foo1 2 2>> obs.err  >> obs.txt
	./nexus foo2 2 2>> obs.err  >> obs.txt

clean:
	rm -f nexus* pool*
