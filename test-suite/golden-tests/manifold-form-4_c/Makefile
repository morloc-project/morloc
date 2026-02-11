all:
	rm -f obs.txt
	morloc make t4.loc 2> build.err
	./nexus foo > obs.txt 2> obs.err

clean:
	rm -f nexus* *out pool*
