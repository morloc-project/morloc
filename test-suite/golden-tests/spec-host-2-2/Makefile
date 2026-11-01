all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus runPy 1 > obs.txt 2>> obs.err
	./nexus runCpp 2 >> obs.txt 2>> obs.err
	./nexus runR 3 >> obs.txt 2>> obs.err
	./nexus runRust 4 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
