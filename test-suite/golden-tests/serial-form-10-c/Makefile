all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus foo '{"name":"Bob","info":"charming"}' | sed 's/ //g' > obs.txt

clean:
	rm -f nexus* pool*
