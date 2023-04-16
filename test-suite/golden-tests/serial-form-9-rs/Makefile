all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.py foo '[["a","b"],[{"name":"alice","info":2},{"name":"bob","info":3}]]' | sed 's/ //g' > obs.txt

clean:
	rm -rf nexus* *out
