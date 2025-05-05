
all:
	rm -f obs.txt
	morloc make -v t2.loc > log
	./nexus foo "[43,true]" > obs.txt
	./nexus bar 43 true >> obs.txt
	./nexus baz 4.3 -5 >> obs.txt
	./nexus zanzibar [] "{\"a\":5}" >> obs.txt

clean:
	rm -f nexus*
