all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- nameRound ---" > obs.txt
	./nexus nameRound hi >> obs.txt 2>> obs.err
	echo "--- nameCross ---" >> obs.txt
	./nexus nameCross hi >> obs.txt 2>> obs.err
	echo "--- countRound ---" >> obs.txt
	./nexus countRound 41 >> obs.txt 2>> obs.err
	echo "--- aliasRound ---" >> obs.txt
	./nexus aliasRound hi >> obs.txt 2>> obs.err
	echo "--- taggedRound ---" >> obs.txt
	./nexus taggedRound hi >> obs.txt 2>> obs.err
	echo "--- stackRev ---" >> obs.txt
	./nexus stackRev '[1,2,3]' >> obs.txt 2>> obs.err
	echo "--- stackCross ---" >> obs.txt
	./nexus stackCross '[1,2,3]' >> obs.txt 2>> obs.err
	echo "--- bufRev ---" >> obs.txt
	./nexus bufRev '[1,2,3]' >> obs.txt 2>> obs.err
	echo "--- bufCross ---" >> obs.txt
	./nexus bufCross '[1,2,3]' >> obs.txt 2>> obs.err
	echo "--- pairSwap ---" >> obs.txt
	./nexus pairSwap '["a",7]' >> obs.txt 2>> obs.err
	echo "--- pathSuffix ---" >> obs.txt
	./nexus pathSuffix notes/report.txt >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err __pycache__
	rm -rf *-build *build.tmp*
