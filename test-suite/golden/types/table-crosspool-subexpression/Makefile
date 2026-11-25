all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus width 4 > obs.txt 2> obs.err
	./nexus widthPlus 4 >> obs.txt 2>> obs.err
	./nexus bothWidths 4 >> obs.txt 2>> obs.err
	./nexus viaLet 4 >> obs.txt 2>> obs.err

clean:
	rm -f *.err nexus
	rm -rf *-build *build.tmp* __pycache__
