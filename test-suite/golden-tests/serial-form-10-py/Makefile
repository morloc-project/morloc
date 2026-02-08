all:
	rm -f obs.txt
	morloc make foo.loc 
	./nexus foo '{"name":"Bob","info":"charming"}' | sed 's/ //g' > obs.txt

clean:
	rm -f nexus* pool*
