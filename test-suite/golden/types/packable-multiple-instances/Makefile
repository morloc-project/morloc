all:
	rm -f *.err obs.txt
	morloc make -o disjoint disjoint.loc 2> build.err
	morloc make -o specialize specialize.loc 2>> build.err
	morloc make -o pairing pairing.loc 2>> build.err
	morloc make -o natgeneric nat-generic.loc 2>> build.err
	echo "--- disjoint ---" >> obs.txt
	./disjoint howInt '[1,2]' >> obs.txt 2> obs.err
	./disjoint howStr '["a"]' >> obs.txt 2>> obs.err
	echo "--- specialize ---" >> obs.txt
	./specialize howInt '[1,2]' >> obs.txt 2>> obs.err
	./specialize howStr '["a"]' >> obs.txt 2>> obs.err
	echo "--- pairing ---" >> obs.txt
	./pairing mkTree a >> obs.txt 2>> obs.err
	echo "--- nat-generic ---" >> obs.txt
	./natgeneric howVec4 '[1,2,3,4]' >> obs.txt 2>> obs.err

clean:
	rm -f *.err
	rm -rf disjoint specialize pairing natgeneric __pycache__
	rm -rf *-build *build.tmp*
