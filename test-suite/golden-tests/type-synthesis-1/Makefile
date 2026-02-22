all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo 42 > obs.txt 2> obs.err
	./nexus foos 42 2>> obs.err  >> obs.txt
	./nexus sfoos '[42]' 2>> obs.err  >> obs.txt
	./nexus bar 42 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus pools
