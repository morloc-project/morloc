all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus scaleThree > obs.txt 2> obs.err
	./nexus halveInt 7 >> obs.txt 2>> obs.err
	./nexus twiceBoth 1 a >> obs.txt 2>> obs.err
	./nexus optId 5 >> obs.txt 2>> obs.err
	./nexus --keep-null optId null >> obs.txt 2>> obs.err
	./nexus countdownPlus 4 >> obs.txt 2>> obs.err
	./nexus labelReal 1.5 >> obs.txt 2>> obs.err
	./nexus labelCelsius 1.5 >> obs.txt 2>> obs.err
	./nexus doubleThree >> obs.txt 2>> obs.err

clean:
	rm -rf nexus __pycache__ *.err
	rm -rf *-build *build.tmp*
