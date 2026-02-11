all:
	rm -f obs.txt
	morloc make t7.loc 2> build.err
	./nexus foo 16 > obs.txt 2> obs.err

clean:
	rm -f nexus* pool*
