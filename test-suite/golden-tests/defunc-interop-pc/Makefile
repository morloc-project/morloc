all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus crossApply >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
