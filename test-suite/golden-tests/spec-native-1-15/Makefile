all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus readPt '{"fst":12,"snd":5}' >> obs.txt 2>> obs.err
	./nexus mkPt 7 >> obs.txt 2>> obs.err
	./nexus readPts '[{"fst":12,"snd":5},{"fst":1,"snd":1}]' >> obs.txt 2>> obs.err
	./nexus readOpt '{"fst":12,"snd":5}' >> obs.txt 2>> obs.err
	./nexus readOpt null >> obs.txt 2>> obs.err
	./nexus readW '{"fst":[12,3],"snd":[4,5]}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *err *-build *build.tmp*
