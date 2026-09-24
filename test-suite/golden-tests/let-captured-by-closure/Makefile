all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus pureLet ab > obs.txt 2> obs.err
	./nexus effectLet ab >> obs.txt 2>> obs.err
	./nexus computedArg ab >> obs.txt 2>> obs.err

clean:
	rm -rf nexus __pycache__ *.err
	rm -rf *-build *build.tmp*
