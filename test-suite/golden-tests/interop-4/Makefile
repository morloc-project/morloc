all:
	rm -f obs.txt
	morloc make main.loc 2> build.err
	./nexus foo 9 > obs.txt 2> obs.err

clean:
	rm -f nexus* pool* *pdf
