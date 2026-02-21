all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	>obs.txt
	./nexus foop a.json 2>> obs.err  >> obs.txt
	./nexus foor a.json 2>> obs.err  >> obs.txt
	./nexus fooc a.json 2>> obs.err  >> obs.txt
	./nexus foop b.json 2>> obs.err  >> obs.txt
	./nexus foor b.json 2>> obs.err  >> obs.txt
	./nexus fooc b.json 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus pools __pycache__
