all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.pl foo '[["b","a"],[55,42]]' | sed 's/ //g' > obs.txt

clean:
	rm -rf nexus* *out
