all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus pyInt 5 > obs.txt 2> obs.err
	./nexus pyOpt 5 >> obs.txt 2>> obs.err
	./nexus pyOpt null >> obs.txt 2>> obs.err
	./nexus cppInt 5 >> obs.txt 2>> obs.err
	./nexus cppOpt 5 >> obs.txt 2>> obs.err
	./nexus cppOpt null >> obs.txt 2>> obs.err
	./nexus rInt 5 >> obs.txt 2>> obs.err
	./nexus rOpt 5 >> obs.txt 2>> obs.err
	./nexus rOpt null >> obs.txt 2>> obs.err
	./nexus pyHeadNull '[null, 2]' >> obs.txt 2>> obs.err
	./nexus pyHeadNull '[3]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
