all:
	rm -f *.err obs.txt *.mpk
	morloc make -o nexus main.loc 2> build.err
	./nexus roundTrip t.mpk 5 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err *.mpk *-build *build.tmp*
