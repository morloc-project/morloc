all:
	rm -f obs.txt
	morloc make foo.loc 2> build.err
	./nexus r2c 3 4 > obs.txt 2> obs.err
	./nexus c2r 3 4 2>> obs.err  >> obs.txt

clean:
	rm -f nexus* pool*
