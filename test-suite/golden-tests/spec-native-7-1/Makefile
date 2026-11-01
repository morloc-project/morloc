all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus litBytes > obs.txt 2> obs.err
	./nexus cppBytes 3 >> obs.txt 2>> obs.err
	./nexus litDirs >> obs.txt 2>> obs.err
	./nexus cppDirs >> obs.txt 2>> obs.err
	./nexus roundTrip 3 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *err
	rm -rf *-build *build.tmp*
