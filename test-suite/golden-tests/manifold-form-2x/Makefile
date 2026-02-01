
all:
	rm -f obs.txt
	morloc make t2.loc 
	./nexus foo 99 > obs.txt
	./nexus bar true '"Alice"' >> obs.txt
	./nexus baz 4.3 -5 >> obs.txt

clean:
	rm -f nexus*
