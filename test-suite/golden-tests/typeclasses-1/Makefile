all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus foo '"a"' '"b"' > obs.txt
	./nexus bar 6 5 >> obs.txt

clean:
	rm -f nexus* pool*
