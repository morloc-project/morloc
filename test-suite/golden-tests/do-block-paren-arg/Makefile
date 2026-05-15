all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus solo > obs.txt 2> obs.err
	./nexus nested >> obs.txt 2>> obs.err
	./nexus explicitBrace >> obs.txt 2>> obs.err
	./nexus plainParen >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err __pycache__
