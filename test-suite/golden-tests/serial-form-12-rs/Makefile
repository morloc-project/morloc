all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.pl foo '{"name":"alice","info":{"name":"bob","info":42}}' | sed 's/ //g' > obs.txt

clean:
	rm -rf nexus* *out
