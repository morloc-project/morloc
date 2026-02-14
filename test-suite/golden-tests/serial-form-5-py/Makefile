all:
	rm -f *.err obs.txt
	morloc make -o nexus foo.loc 2> build.err
	./nexus foo '[[["a","b"],[1,5]],[["c","d"],[2,6]]]' | sed 's/ //g' > obs.txt 2> obs.err

clean:
	rm -f nexus
