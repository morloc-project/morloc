all:
	rm -f obs.txt
	morloc make foo.loc
	./nexus.pl foo '{"name":"Bob","info":"charming"}' > obs.txt

clean:
	rm -f nexus* pool*
