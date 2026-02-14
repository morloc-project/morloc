all:
	rm -f obs.txt
	morloc make high.loc 2> build.err
	./nexus foo [99,2] > obs.txt 2> obs.err

clean:
	rm -f nexus* pool*
