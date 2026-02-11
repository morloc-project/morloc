all:
	rm -f obs.txt
	morloc make t5.loc 2> build.err
	./nexus foo > obs.txt 2> obs.err

clean:
	rm -f nexus* pool*
