all:
	rm -f obs.txt *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus foldRs --n 50 > obs.txt 2>> obs.err
	./nexus foldCpp --n 50 >> obs.txt 2>> obs.err
	./nexus foldRs2 --n 50 >> obs.txt 2>> obs.err
	./nexus foldCpp2 --n 50 >> obs.txt 2>> obs.err
	./nexus foldAny --n 50 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err
	rm -rf *-build *build.tmp*
