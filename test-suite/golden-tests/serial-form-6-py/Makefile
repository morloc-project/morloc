all:
	rm -f obs.txt
	morloc make foo.loc 2> build.err
	./nexus foo '[{"name":"alice","info":42},{"name":"bob","info":43}]' | sed 's/ //g' > obs.txt 2> obs.err

clean:
	rm -f nexus* pool*
