all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus foo '[["b","a"],[55,42]]' | sed 's/ //g' > obs.txt

clean:
	rm -f nexus* pool*
