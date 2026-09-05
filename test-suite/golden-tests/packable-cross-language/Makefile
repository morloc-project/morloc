all:
	rm -f *.err obs.txt
	morloc make -o twosources two-sources.loc 2> build.err
	morloc make -o perlanguage per-language.loc 2>> build.err
	morloc make -o crosspool crosspool.loc 2>> build.err
	echo "--- two-sources ---" >> obs.txt
	./twosources howPy '[1,2,3]' >> obs.txt 2> obs.err
	./twosources howCpp '[1,2,3]' >> obs.txt 2>> obs.err
	echo "--- per-language ---" >> obs.txt
	./perlanguage howPy '[1,2,3]' >> obs.txt 2>> obs.err
	./perlanguage howCpp '[1,2,3]' >> obs.txt 2>> obs.err
	echo "--- crosspool ---" >> obs.txt
	./crosspool roundTrip '[3,4]' >> obs.txt 2>> obs.err

clean:
	rm -f *.err
	rm -rf twosources perlanguage crosspool __pycache__
	rm -rf *-build *build.tmp*
