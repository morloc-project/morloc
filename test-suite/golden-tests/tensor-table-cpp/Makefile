all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus result > obs.txt 2> obs.err
	./nexus cellCount >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
	rm -rf *-build *build.tmp*
