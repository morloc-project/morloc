all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.pl foo '{"name":"Bob","info":"charming"}' | sed 's/ //g' > obs.txt

clean:
	rm -rf nexus* *out
