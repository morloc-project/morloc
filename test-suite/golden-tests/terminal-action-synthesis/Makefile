all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus flat 1 2 >> obs.txt 2>> obs.err
	./nexus flat -y 1 2 >> obs.txt 2>> obs.err
	./nexus grouped 1 2 >> obs.txt 2>> obs.err
	./nexus grouped -y 1 2 >> obs.txt 2>> obs.err
	./nexus grouped -t 1 2 >> obs.txt 2>> obs.err
	./nexus effectAlias 1 >> obs.txt 2>> obs.err
	./nexus effectAlias -y 1 >> obs.txt 2>> obs.err
	./nexus fnAlias 1 2 >> obs.txt 2>> obs.err
	./nexus fnAlias -y 1 2 >> obs.txt 2>> obs.err
	./nexus lambdaParam -r 1 2 >> obs.txt 2>> obs.err
	./nexus shadowedParam -r 1 2 >> obs.txt 2>> obs.err
	./nexus importedBatch -a 3 >> obs.txt 2>> obs.err
	./nexus importedHandler -a 3 >> obs.txt 2>> obs.err
	./nexus fileAlias -m 3 >> obs.txt 2>> obs.err
	./nexus letShadow -r 7 >> obs.txt 2>> obs.err
	./nexus nestedImported -a 3 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
