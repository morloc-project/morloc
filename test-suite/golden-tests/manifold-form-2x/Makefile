
all:
	rm -f obs.txt
	morloc make -v t2.loc > log
	./nexus foo 99 > obs.txt
	./nexus bar true '"Alice"' >> obs.txt
	./nexus baz 4.3 -5 >> obs.txt

clean:
	rm -f nexus*
