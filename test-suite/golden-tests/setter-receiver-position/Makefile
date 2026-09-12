all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus setLast  >  obs.txt 2>  obs.err
	./nexus setFirst >> obs.txt 2>> obs.err
	./nexus plainGet >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
	rm -f *.err
	rm -rf *-build *build.tmp*
