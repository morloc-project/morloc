all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus.pl rms [1,2,3] > obs.txt

clean:
	rm -rf nexus* *out
