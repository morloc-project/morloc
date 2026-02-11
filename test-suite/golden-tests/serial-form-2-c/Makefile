all:
	rm -f obs.txt
	morloc make foo.loc 2> build.err
	./nexus foo '[["b","a"],[55,42]]' > obs.txt 2> obs.err

clean:
	rm -f nexus* pool*
