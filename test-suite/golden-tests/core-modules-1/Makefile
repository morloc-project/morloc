all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus foo '"Alice"' 8 > obs.txt

clean:
	rm -f nexus* pool*
