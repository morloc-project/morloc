all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	>obs.txt
	./nexus hello 2>> obs.err >> obs.txt
	./nexus chinese 2>> obs.err >> obs.txt
	./nexus mixed 2>> obs.err >> obs.txt

clean:
	rm -rf nexus pools __pycache__
