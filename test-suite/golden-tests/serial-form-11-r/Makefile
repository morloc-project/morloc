all:
	rm -f obs.txt
	morloc make foo.loc
	./nexus.pl foo '\{\"name\":\"alice\",\"info\":[[\"a\",\"b\"],[1,2]]\}' > obs.txt

clean:
	rm -f nexus* pool*
