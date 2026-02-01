all:
	rm -f obs.txt
	morloc make foo.loc 
	./nexus foo '[12345678,987654321]' >> obs.txt

clean:
	rm -f nexus* pool*
