all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	>obs.txt
	./nexus foop a.txt 2>> obs.err  >> obs.txt
	./nexus foor a.txt 2>> obs.err  >> obs.txt
	./nexus fooc a.txt 2>> obs.err  >> obs.txt
	./nexus foop b.txt 2>> obs.err  >> obs.txt
	./nexus foor b.txt 2>> obs.err  >> obs.txt
	./nexus fooc b.txt 2>> obs.err  >> obs.txt


clean:
	rm -rf nexus pools __pycache__
