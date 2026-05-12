all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	>obs.txt
	./nexus pyMake        2>> obs.err >> obs.txt
	./nexus cppMake       2>> obs.err >> obs.txt
	./nexus pyMakeLen     2>> obs.err >> obs.txt
	./nexus cppMakeLen    2>> obs.err >> obs.txt
	./nexus pp            2>> obs.err >> obs.txt
	./nexus pc            2>> obs.err >> obs.txt
	./nexus cp            2>> obs.err >> obs.txt
	./nexus cc            2>> obs.err >> obs.txt
	./nexus ppLen         2>> obs.err >> obs.txt
	./nexus pcLen         2>> obs.err >> obs.txt
	./nexus cpLen         2>> obs.err >> obs.txt
	./nexus ccLen         2>> obs.err >> obs.txt
	./nexus pureNul       2>> obs.err >> obs.txt
	./nexus pureNulLen    2>> obs.err >> obs.txt
	./nexus showPureNul   2>> obs.err >> obs.txt

clean:
	rm -rf nexus pools __pycache__
