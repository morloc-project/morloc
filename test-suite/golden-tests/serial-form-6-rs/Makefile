all:
	rm -f obs.txt
	morloc make foo.loc
	./nexus.pl foo '[{"name":"alice","info":42},{"name":"bob","info":43}]' > obs.txt

clean:
	rm -rf nexus* *out
