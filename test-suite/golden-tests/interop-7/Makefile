all:
	rm -f obs.txt
	morloc make foo.loc 2> build.err
	./nexus foo 13 > obs.txt 2> obs.err
	diff exp.txt obs.txt

clean:
	rm -f nexus* pool*
