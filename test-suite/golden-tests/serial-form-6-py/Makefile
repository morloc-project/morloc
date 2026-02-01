all:
	rm -f obs.txt
	morloc make foo.loc 
	./nexus foo '[{"name":"alice","info":42},{"name":"bob","info":43}]' | sed 's/ //g' > obs.txt

clean:
	rm -f nexus* pool*
