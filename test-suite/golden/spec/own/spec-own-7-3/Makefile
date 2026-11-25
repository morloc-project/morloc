all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus temps 2 >> obs.txt 2>> obs.err
	./nexus lastRead 2 >> obs.txt 2>> obs.err
	./nexus callerKeeps 2 >> obs.txt 2>> obs.err
	./nexus mixed 2 >> obs.txt 2>> obs.err
	./nexus dropsLive 2 >> obs.txt 2>> obs.err
	./nexus twice 2 >> obs.txt 2>> obs.err
	./nexus asValue 2 >> obs.txt 2>> obs.err
	./nexus releases 2 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
	rm -f *.err
	rm -rf *-build *build.tmp*
