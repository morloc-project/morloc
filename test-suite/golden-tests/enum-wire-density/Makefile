all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- sizeAsEnum ---" >> obs.txt
	./nexus sizeAsEnum '["A","C","G","T"]' >> obs.txt 2>> obs.err
	echo "--- sizeAsBytes ---" >> obs.txt
	./nexus sizeAsBytes '[0,1,2,3]' >> obs.txt 2>> obs.err
	echo "--- sizeParity ---" >> obs.txt
	./nexus sizeParity '["A","C","G","T"]' '[0,1,2,3]' >> obs.txt 2>> obs.err
	echo "--- countPurines ---" >> obs.txt
	./nexus countPurines '["A","C","G","T","A","A","G"]' >> obs.txt 2>> obs.err
	echo "--- decodeHead ---" >> obs.txt
	./nexus decodeHead '["G","C","A"]' >> obs.txt 2>> obs.err
	echo "--- roundTripLength ---" >> obs.txt
	./nexus roundTripLength '["A","C","G","T","A","C"]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
	rm -rf *-build *build.tmp*
