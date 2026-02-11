all:
	rm -f obs.txt
	morloc make main.loc 2> build.err
	./nexus foo '"a"' '"b"' > obs.txt 2> obs.err
	./nexus bar 6 5 2>> obs.err  >> obs.txt

clean:
	rm -f nexus* pool*
