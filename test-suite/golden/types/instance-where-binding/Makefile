all:
	rm -f *.err obs.txt tick.log
	morloc make -o nexus main.loc 2> build.err
	rm -f tick.log; ./nexus scaleInt 3 > obs.txt 2> obs.err
	rm -f tick.log; ./nexus countInt 3 >> obs.txt 2>> obs.err
	./nexus scaleReal 1.5 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus __pycache__ *.err tick.log
	rm -rf *-build *build.tmp*
