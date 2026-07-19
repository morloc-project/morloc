all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus setSingleHet    >  obs.txt 2> obs.err
	./nexus setTwoHet       >> obs.txt 2>> obs.err
	./nexus setLastHet      >> obs.txt 2>> obs.err
	./nexus setReorderArgs  >> obs.txt 2>> obs.err
	./nexus setMixedNested  >> obs.txt 2>> obs.err
	./nexus setI32Lit     >> obs.txt 2>> obs.err
	./nexus setU32Lit    >> obs.txt 2>> obs.err
	./nexus setMixedWidthTwo >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
