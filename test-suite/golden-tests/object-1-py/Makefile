all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus foo '{"name":"alice","info":34}' > obs.txt

clean:
	rm -f nexus* pool*
