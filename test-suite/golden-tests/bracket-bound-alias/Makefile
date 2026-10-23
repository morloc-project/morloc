all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus nexusIndex '[10,20,30]' 1 >> obs.txt 2>> obs.err
	./nexus nexusSlice '[10,20,30]' 1 >> obs.txt 2>> obs.err
	./nexus nexusOpt '[10,20,30]' 2 >> obs.txt 2>> obs.err
	./nexus nexusOpt '[10,20,30]' null >> obs.txt 2>> obs.err
	./nexus pyIndex '[10,20,30]' 1 >> obs.txt 2>> obs.err
	./nexus cppIndex '[10,20,30]' 0 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
