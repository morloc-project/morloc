all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.py foo '{"name":["alice","bob"],"info":[34,35]}' > obs.txt

clean:
	rm -f nexus* pool*
