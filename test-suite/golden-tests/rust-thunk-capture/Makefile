all:
	rm -f *.err obs.txt rt.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus afterSave rt.dat >> obs.txt 2> obs.err
	./nexus afterCatch missing.dat >> obs.txt 2>> obs.err
	./nexus bothArms missing.dat >> obs.txt 2>> obs.err
	./nexus recordReuse rt.dat >> obs.txt 2>> obs.err
	./nexus loopReuse rt.dat 3 >> obs.txt 2>> obs.err
	./nexus afterSave rt.dat > /dev/null 2>> obs.err
	./nexus letThunk rt.dat >> obs.txt 2>> obs.err
	./nexus guardedTwice missing.dat >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err rt.dat
	rm -rf *-build *build.tmp*
