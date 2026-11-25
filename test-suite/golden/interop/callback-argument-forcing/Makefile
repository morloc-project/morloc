all:
	rm -f *.err obs.txt note.log
	morloc make -o nexus main.loc 2> build.err
	./nexus once 10 > obs.txt 2> obs.err
	./nexus twiceUsed 20 >> obs.txt 2>> obs.err
	./nexus composed 30 >> obs.txt 2>> obs.err
	./nexus throughWrappers 40 >> obs.txt 2>> obs.err
	cat note.log >> obs.txt

clean:
	rm -rf nexus __pycache__ *.err note.log
	rm -rf *-build *build.tmp*
