all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.pl foo '[{"name":"alice","info":42},{"name":"bob","info":43}]' > obs.txt

clean:
	rm -rf nexus* *out
