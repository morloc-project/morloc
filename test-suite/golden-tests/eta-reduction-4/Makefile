all:
	rm -f obs.txt
	morloc make main.loc 2> build.err
	./nexus foo "[1.0,2.0,3.0]" > obs.txt 2> obs.err

clean:
	rm -f nexus* pool*
