P = '{"name":"a","age":3,"inner":{"label":"x","tags":["t"]}}'
all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus viaPy $(P) > obs.txt 2> obs.err
	./nexus viaCpp $(P) >> obs.txt 2>> obs.err
	./nexus older $(P) >> obs.txt 2>> obs.err
	./nexus kind $(P) >> obs.txt 2>> obs.err
	./nexus mk bob >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *err *-build *build.tmp*
