all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus rms [2,4,4,8] > obs.txt

clean:
	rm -f nexus* pool*
