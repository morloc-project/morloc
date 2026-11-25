all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus len 1 > obs.txt 2>> obs.err
	./nexus len 3000 >> obs.txt 2>> obs.err
	./nexus total 100 >> obs.txt 2>> obs.err
	./nexus --keep-null second 5 >> obs.txt 2>> obs.err
	./nexus --keep-null second 1 >> obs.txt 2>> obs.err
	./nexus lastIsNull 0 >> obs.txt 2>> obs.err
	./nexus --keep-null third 5 >> obs.txt 2>> obs.err
	./nexus --keep-null third 2 >> obs.txt 2>> obs.err
	./nexus cut 9 >> obs.txt 2>> obs.err
	./nexus relink 7 >> obs.txt 2>> obs.err
	./nexus --keep-null nextOf 3 >> obs.txt 2>> obs.err
	./nexus keepBoth 5 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *-build *build.tmp*
	rm -f *.err
