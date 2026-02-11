all:
	rm -f obs.txt
	morloc make main.loc 2> build.err
	./nexus foo 2 [1,2,3] [4,5,6] > obs.txt 2> obs.err

clean:
	rm -f nexus* pool* *pdf
