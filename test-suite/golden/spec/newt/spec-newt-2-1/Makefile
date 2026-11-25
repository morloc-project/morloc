all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus shout hi 2>> obs.err >> obs.txt
	./nexus nameLen hello 2>> obs.err >> obs.txt
	./nexus pointSum '[3,4]' 2>> obs.err >> obs.txt
	./nexus pointLit 3 2>> obs.err >> obs.txt

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
