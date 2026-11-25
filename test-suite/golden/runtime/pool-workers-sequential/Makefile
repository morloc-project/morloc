all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus fromCpp 2000 >> obs.txt 2>> obs.err
	./nexus fromPy 2000 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err __pycache__
	rm -rf *-build *build.tmp*
