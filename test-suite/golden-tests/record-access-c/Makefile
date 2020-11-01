all:
	rm -f obs.txt
	morloc make foo.loc
	# ./nexus.pl foo '\{\"name\":[\"alice\",\"bob\"],\"info\":[34,35]\}' > obs.txt
	./nexus.pl foo '{"name":"alice","info":34}' > obs.txt

clean:
	rm -f nexus* pool*
