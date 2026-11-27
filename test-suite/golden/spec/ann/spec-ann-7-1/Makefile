all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo '--- letBody ---' >> obs.txt
	./nexus letBody '3' >> obs.txt 2> letBody.err; sort letBody.err >> obs.txt
	echo '--- guardBody ---' >> obs.txt
	./nexus guardBody '3' >> obs.txt 2> guardBody.err; sort guardBody.err >> obs.txt
	echo '--- matchBody ---' >> obs.txt
	./nexus matchBody '3' >> obs.txt 2> matchBody.err; sort matchBody.err >> obs.txt
	echo '--- opBody ---' >> obs.txt
	./nexus opBody '3' >> obs.txt 2> opBody.err; sort opBody.err >> obs.txt
	echo '--- recOuter ---' >> obs.txt
	./nexus recOuter '[1,2,3,4,5]' >> obs.txt 2> recOuter.err; sort recOuter.err >> obs.txt
	echo '--- recInner ---' >> obs.txt
	./nexus recInner '[1,2,3]' >> obs.txt 2> recInner.err; sort recInner.err >> obs.txt
	echo '--- bindIO ---' >> obs.txt
	./nexus bindIO '1' >> obs.txt 2> bindIO.err; sort bindIO.err >> obs.txt
	echo '--- letIO ---' >> obs.txt
	./nexus letIO '1' >> obs.txt 2> letIO.err; sort letIO.err >> obs.txt
	echo '--- bangIO ---' >> obs.txt
	./nexus bangIO '1' >> obs.txt 2> bangIO.err; sort bangIO.err >> obs.txt
	echo '--- finalIO ---' >> obs.txt
	./nexus finalIO '1' >> obs.txt 2> finalIO.err; sort finalIO.err >> obs.txt
	echo '--- whereHelper ---' >> obs.txt
	./nexus whereHelper '1' >> obs.txt 2> whereHelper.err; sort whereHelper.err >> obs.txt
	echo '--- mapped ---' >> obs.txt
	./nexus mapped '[1,2,3]' >> obs.txt 2> mapped.err; sort mapped.err >> obs.txt
	echo '--- partial ---' >> obs.txt
	./nexus partial '[1,2,3]' >> obs.txt 2> partial.err; sort partial.err >> obs.txt
	echo '--- inTuple ---' >> obs.txt
	./nexus inTuple '2' >> obs.txt 2> inTuple.err; sort inTuple.err >> obs.txt
	echo '--- inBranch ---' >> obs.txt
	./nexus inBranch '2' >> obs.txt 2> inBranch.err; sort inBranch.err >> obs.txt
	echo '--- crossLang ---' >> obs.txt
	./nexus crossLang '2' >> obs.txt 2> crossLang.err; sort crossLang.err >> obs.txt
	echo '--- lambdaBound ---' >> obs.txt
	./nexus lambdaBound '2' >> obs.txt 2> lambdaBound.err; sort lambdaBound.err >> obs.txt
	echo '--- letBound ---' >> obs.txt
	./nexus letBound '2' >> obs.txt 2> letBound.err; sort letBound.err >> obs.txt
	echo '--- tailRec ---' >> obs.txt
	./nexus tailRec '4' >> obs.txt 2> tailRec.err; sort tailRec.err >> obs.txt
	echo '--- staged ---' >> obs.txt
	./nexus staged '[1,2,3]' >> obs.txt 2> staged.err; sort staged.err >> obs.txt

clean:
	rm -rf nexus __pycache__ *.err
	rm -rf *-build *build.tmp*
