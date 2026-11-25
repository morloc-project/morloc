all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus flipUB true >> obs.txt 2>> obs.err
	./nexus flipUB false >> obs.txt 2>> obs.err
	./nexus bumpUI 300 >> obs.txt 2>> obs.err
	./nexus lastUnit 7 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
