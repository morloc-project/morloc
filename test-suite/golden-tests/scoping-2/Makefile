all:
	rm -f obs.txt
	morloc make w.loc 2> build.err
	./nexus f 42 > obs.txt 2> obs.err

clean:
	rm -f nexus* pool*
