all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus onceRust 5 >> obs.txt 2>> obs.err
	./nexus twiceRust 5 >> obs.txt 2>> obs.err
	./nexus pairRust 5 >> obs.txt 2>> obs.err
	./nexus loopRust 10 0 >> obs.txt 2>> obs.err
	./nexus onceCpp 5 >> obs.txt 2>> obs.err
	./nexus twiceCpp 5 >> obs.txt 2>> obs.err
	./nexus pairCpp 5 >> obs.txt 2>> obs.err
	./nexus loopCpp 10 0 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
