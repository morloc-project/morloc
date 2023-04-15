all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.py foo '{"name":"alice","info":34}' > obs.txt

clean:
	rm -rf nexus* *out
