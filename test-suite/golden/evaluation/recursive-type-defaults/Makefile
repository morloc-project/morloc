all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus depth >> obs.txt 2>> obs.err
	./nexus pats >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err __pycache__
	rm -rf *-build *build.tmp*
