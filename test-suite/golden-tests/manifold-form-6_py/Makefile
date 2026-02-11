all:
	rm -f obs.txt
	morloc make t6.loc 2> build.err
	./nexus foo 16 > obs.txt 2> obs.err

clean:
	rm -f nexus* pool.py
