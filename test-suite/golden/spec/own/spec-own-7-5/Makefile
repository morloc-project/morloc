all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus ownedLet 4 >> obs.txt 2>> obs.err
	./nexus lambda 4 >> obs.txt 2>> obs.err
	./nexus partial 4 >> obs.txt 2>> obs.err
	./nexus labeled 4 >> obs.txt 2>> obs.err
	./nexus chain 4 >> obs.txt 2>> obs.err
	./nexus named 4 >> obs.txt 2>> obs.err
	./nexus given 4 >> obs.txt 2>> obs.err
	./nexus givenLabeled 4 >> obs.txt 2>> obs.err
	./nexus lent 4 >> obs.txt 2>> obs.err
	./nexus stdFunction 4 >> obs.txt 2>> obs.err
	./nexus readLater 4 >> obs.txt 2>> obs.err
	./nexus sameStep 4 >> obs.txt 2>> obs.err
	./nexus captured 4 >> obs.txt 2>> obs.err
	./nexus loop 4 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
	rm -f *.err
	rm -rf *-build *build.tmp*
