all:
	rm -f obs.txt
	morloc make main.loc 2> build.err
	./nexus foo 2 2>> obs.err  >> obs.txt

clean:
	rm -f nexus* pool*
