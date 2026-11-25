all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus capInt 5 '[1,2,3,4,5,6,7,8,9]' >> obs.txt 2>> obs.err
	./nexus capStr p- '["a","b","c","d","e"]' >> obs.txt 2>> obs.err
	./nexus capRec 2 6 '[0,1,2,3,4,5,6,7,8,9]' >> obs.txt 2>> obs.err
	./nexus partial 3 '[1,2,3,4,5,6,7,8,9]' >> obs.txt 2>> obs.err
	./nexus capFun 3 '[1,2,3,4,5]' >> obs.txt 2>> obs.err
	./nexus fnList 7 >> obs.txt 2>> obs.err
	./nexus fieldFn 4 '[1,2,3,4,5]' >> obs.txt 2>> obs.err
	./nexus hostFn 10 '[1,2,3,4,5]' >> obs.txt 2>> obs.err
	./nexus foreignCap 1000 '[1,2,3,4,5,6,7,8,9]' >> obs.txt 2>> obs.err
	./nexus treeSizes 4 >> obs.txt 2>> obs.err
	./nexus treeBuild '[0,1,2,3,4,5,6,7]' >> obs.txt 2>> obs.err
	./nexus tryThrow 6 '[1,2,3,4,5,6,7,8,9]' >> obs.txt 2>> obs.err
	./nexus tryThrow 60 '[1,2,3,4,5,6,7,8,9]' >> obs.txt 2>> obs.err
	./nexus tryForeign 6 '[1,2,3,4,5,6,7,8,9]' >> obs.txt 2>> obs.err
	./nexus tryForeign 60 '[1,2,3,4,5,6,7,8,9]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
