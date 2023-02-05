all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.pl foo '[{"name":"alice","info":42},{"name":"bob","info":43}]' | sed 's/ //g' > obs.txt

clean:
	rm -f nexus* pool*
