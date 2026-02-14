all:
	rm -f obs.txt
	morloc make t2.loc 2> build.err
	./nexus foo 99 > obs.txt 2> obs.err
	./nexus bar true '"Alice"' 2>> obs.err  >> obs.txt
	./nexus baz 4.3 -5 2>> obs.err  >> obs.txt

clean:
	rm -f nexus*
