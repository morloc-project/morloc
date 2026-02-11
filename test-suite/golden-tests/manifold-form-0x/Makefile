all:
	rm -f obs.txt
	morloc make foo.loc 2> build.err
	./nexus foo 12 > obs.txt 2> obs.err

clean:
	rm -f nexus* pool* *pdf
