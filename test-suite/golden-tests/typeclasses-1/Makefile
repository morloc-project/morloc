all:
	rm -f obs.txt
	morloc make main.loc 
	./nexus foo '"a"' '"b"' > obs.txt
	./nexus bar 6 5 >> obs.txt

clean:
	rm -f nexus* pool*
